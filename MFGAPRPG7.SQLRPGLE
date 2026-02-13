     *-------------------------------------------------------------        *
     * Program Name  : MFGAPRPG7                                           *
     * Description   : Manufacturing Inventory Management System           *
     *                                                                     *
     *                                                                     *
     * Developed By   : Archit Tambi                                       *
     * Role           : IBMi Intern                                        *
     * Language       : RPGLE (Free / Fixed \Z as applicable)              *
     * Created At     : 1/25/2026                                          *
     *                                                                  *
     *--------------------------------------------------------------------*
     **free
         Ctl-opt Nomain;


     ** ¬ FILE DECLARATION |
         Dcl-f Mfg01dsp Workstn Sfile(Locswin:Wlocrrn)
                 Sfile(Mngspro:Prorrn) Sfile(Prodsdel:Prodrrn)
                 Indds(Indds) Infds(Subinfo);

     ** ¬INDICATOR DATA STRUCTURE DECLARATION |
         Dcl-ds Indds;
           Exit          Ind Pos(03);
           Prompt        Ind Pos(04);
           Refresh       Ind Pos(05);
           Actinact      Ind Pos(07);
           Cancel        Ind Pos(12);
           Resete        Ind Pos(11);
           Wchorv        Ind Pos(14);
           Crupd         Ind Pos(06);
           Prosfldsp     Ind Pos(58);
           Prosflctl     Ind Pos(59);
           Prosflclr     Ind Pos(60);
           Prosflend     Ind Pos(61);
           Optind        Ind Pos(28);
           Pronmrp       Ind Pos(15);
           Protyprp      Ind Pos(16);
           Prostatrp     Ind Pos(17);
           Prolocrp      Ind Pos(18);
           Prodesrp      Ind Pos(19);
           Proprtypnd    Ind Pos(20);
           Prostatprnd   Ind Pos(24);
           Proprnd       Ind Pos(21);
           Proptypund    Ind Pos(22);
           Prowinrev     Ind Pos(23);
           Prostatund    Ind Pos(25);
           Proundpr      Ind Pos(26);
           Sflnxtchgind  Ind Pos(30);
           Wlocsfldsp    Ind Pos(50);
           Wlocsflctl    Ind Pos(51);
           Wlocsflclr    Ind Pos(52);
           Wlocsflend    Ind Pos(53);
           Prodsfldsp    Ind Pos(86);
           Prodsflctl    Ind Pos(87);
           Prodsflclr    Ind Pos(88);
           Prodsflend    Ind Pos(89);
           Colourind     Ind Pos(31);
         End-ds;

     ** ¬ PROCEDURE DECLARATION FOR CREATE PRODUCT SCREEN |
         Dcl-pr Productcreate ;
           W_Company Char(32);
           Pgmname   Char(10);
           W_usrid   Packed(6);
         End-pr;

     ** ¬ PROCEDURE DECLARATION FOR DISPLAY PRODUCT SCREEN |
         Dcl-pr Productdsply ;
           W_Company Char(32);
           Pgmname   Char(10);
           W_proid1   Packed(6);
         End-pr;

     ** ¬ PROCEDURE DECLARATION FOR UPDATE PRODUCT SCREEN |
         Dcl-pr Productupdate ;
           W_Company Char(32);
           Pgmname   Char(10);
           W_proid1   Packed(6);
         End-pr;

         Dcl-pr Productdelete;
           W_Company Char(32);
           Pgmname   Char(10);
           W_prodel  Packed(6) Dim(9999);
           W_countdel Packed(6);
         End-pr;

     ** ¬ PROGRAM DESCRIBED DATA STRUCTURE |
         Dcl-ds Subinfo;
           Min_RRN Int(5:0) Pos(378);
         End-ds;


     ** ¬ WORRK VARIABLE DECLARATION  |
         Dcl-s Prorrn        Packed(4:0);
         Dcl-s Wlocrrn       Packed(4:0);
         Dcl-s Prodrrn       Packed(4:0);
         Dcl-s W_proid       Packed(6);
         Dcl-s W_proid1      Packed(6);
         Dcl-s W_proid2      Packed(6) Dim(9999);
         Dcl-s W_prodel      Packed(6) Dim(9999);
         Dcl-s W_proid3      Packed(6);
         Dcl-s W_sqlstmt     varchar(500);
         Dcl-s W_pattern     varchar(50);
         Dcl-s W_pattern1    varchar(50);
         Dcl-s W_count       Packed(1) inz(1);
         Dcl-s W_optcheck    Ind;
         Dcl-s W_optcheck1   Ind;
         Dcl-s W_proactive   Packed(6);
         Dcl-s W_countdel     Packed(6);
         Dcl-s W_countdel1    Packed(6);
         Dcl-s W_prodelid    Packed(6);

         Dcl-proc MngProproc Export;
           Dcl-pi MngProproc;
             W_Company Char(32);
             Pgmname   Char(10);
             W_emptyp  Varchar(1);
             W_usrid   Packed(6);
           End-pi;

           Exec Sql Set Option Commit = *None;
     ** ¬ CALLING CLEARING SUBFILE FOR MANAGE PRODUCT  |
           Exsr Proclrsfl;
     ** ¬ CALLING LOAD SUBFILE FOR MANAGE PRODUCT  |
           Exsr Proloadsfl;
     ** ¬ CALLING DISPLAY SUBFILE FOR MANAGE PRODUCT  |
           Exsr Prodspsfl;

     ** ¬ DECLARING CLEARING SUBFILE FOR MANAGE PRODUCT  |
           Begsr Proclrsfl;
             Prosflclr = *On;
             Prorrn    = 0;
             Write Mngcpro;
             Prosflclr = *Off;
             Sprorrn   = 1;
           Endsr;

     ** ¬ DECLARING LOADING SUBFILE FOR MANAGE PRODUCT  |
            Begsr Proloadsfl;
     ** ¬ FILTERING OPTION FOR PRODUCT NAME  |
              If Sfpronam <> *Blanks and Sfprocat = *Blanks;
                W_pattern='%' + %trim(SFPRONAM) + '%';
                W_sqlstmt = 'Select * FROM MFGL11PRO WHERE PRNAM LIKE ?';
                EXEC SQL
                     Prepare Dynstmt FROM :W_sqlstmt;
                EXEC SQL
                  Declare Procursor1 Scroll Cursor For Dynstmt;
                  Exec Sql Open Procursor1 using :W_pattern;
                Exec Sql
                Fetch First from Procursor1 into
                    :Sproid,:Spronam,:Sprocat,:Sprosta;
                Dow Sqlcode = 0;

                  If Sprosta = 'I';
                   Colourind = *On;
                  Else ;
                    Colourind = *Off;
                  Endif;
     ** ¬ HANDLING ACTIVE/INACTIVE FOR PRODUCT STATUS  |
                If W_count = 2 and Sprosta = 'A';
                Prorrn+=1;
                W_proid2(Prorrn) = %Int(Sproid);
                Write Mngspro;
              Elseif W_count = 3 and Sprosta = 'I';
                 Prorrn+=1;
                W_proid2(Prorrn) = %Int(Sproid);
                Write Mngspro;
              Elseif W_count = 1;
                Prorrn+=1;
                W_proid2(Prorrn) = %Int(Sproid);
                Write Mngspro;
              Endif;
              If Prorrn > 9999;
                Leave;
              Endif;
                // Write Mngspro;
              Exec Sql
                Fetch Next from Procursor1 into
                   :Sproid,:Spronam,:Sprocat,:Sprosta;
            Enddo;
            Exec Sql Close Procursor1;
     ** ¬ FILTERING OPTION FOR PRODUCT CATEGORY  |
            Elseif SFPRONAM <> *Blanks and SFPROCAT <> *Blanks;
                 W_pattern='%' + %trim(SFPROCAT) + '%';
                 W_pattern1='%' + %trim(SFPRONAM) + '%';
                W_sqlstmt = 'SELECT * FROM MFGL11PRO WHERE PRCAT LIKE ?'+
                            ' and PRNAM LIKE ?';
                EXEC SQL
                     PREPARE Dynstmt2 FROM :W_sqlstmt;
                EXEC SQL
                  DECLARE Procursor2 Scroll CURSOR FOR Dynstmt2;
                 Exec Sql Open Procursor2 using :W_pattern,:W_pattern1;
                Exec Sql
                Fetch First from Procursor2 into
                   :Sproid,:Spronam,:Sprocat,:Sprosta;
                Dow Sqlcode=0;
                 If Sprosta = 'I';
                   Colourind = *On;
                 Else ;
                   Colourind = *Off;
                 Endif;
     ** ¬ HANDLING ACTIVE/INACTIVE FOR PRODUCT STATUS  |
                If W_count=2 and Sprosta='A';
                Prorrn+=1;
                W_proid2(Prorrn) = %Int(Sproid);
                Write Mngspro;
              Elseif W_count = 3 and Sprosta='I';
                 Prorrn+=1;
                W_proid2(Prorrn) = %Int(Sproid);
                Write Mngspro;
              Elseif W_count=1;
                Prorrn+=1;
                W_proid2(Prorrn) = %Int(Sproid);
                Write Mngspro;
              Endif;
                If Prorrn>9999;
                  Leave;
                Endif;
                // Write Mngspro;
                Exec Sql
                Fetch Next from Procursor2 into
                   :Sproid,:Spronam,:Sprocat,:Sprosta;
              Enddo;
              Exec Sql Close Procursor2;
     ** ¬ FILTERING OPTION FOR PRODUCT CATEGORY AND PRODUCT NAME  |
              Elseif SFPRONAM = *Blanks and SFPROCAT <> *Blanks;
                 W_pattern='%' + %trim(SFPROCAT) + '%';
                W_sqlstmt = 'SELECT * FROM MFGL11PRO WHERE PRCAT LIKE ?';
                EXEC SQL
                     PREPARE Dynstmt1 FROM :W_sqlstmt;
                EXEC SQL
                  DECLARE Procursor3 Scroll CURSOR FOR Dynstmt1;
                 Exec Sql Open Procursor3 using :W_pattern;
                Exec Sql
                Fetch First from Procursor3 into
                   :Sproid,:Spronam,:Sprocat,:Sprosta;
                Dow Sqlcode=0;
                  If Sprosta = 'I';
                    Colourind = *On;
                  Else ;
                    Colourind = *Off;
                  Endif;
     ** ¬ HANDLING ACTIVE/INACTIVE FOR PRODUCT STATUS  |
                If W_count=2 and Sprosta='A';
                Prorrn+=1;
                W_proid2(Prorrn) = %Int(Sproid);
                Write Mngspro;
              Elseif W_count = 3 and Sprosta='I';
                 Prorrn+=1;
                W_proid2(Prorrn) = %Int(Sproid);
                Write Mngspro;
              Elseif W_count=1;
                Prorrn+=1;
                W_proid2(Prorrn) = %Int(Sproid);
                Write Mngspro;
              Endif;
                If Prorrn>9999;
                  Leave;
                Endif;
                // Write Mngspro;
                Exec Sql
                Fetch Next from Procursor3 into
                   :Sproid,:Spronam,:Sprocat,:Sprosta;
              Enddo;
              Exec Sql Close Procursor3;
     ** ¬FETCHING ALL RECORD FOR PRODUCT MANAGE SCREEN |
              Else;
              Exec Sql
                Declare Procursor Scroll Cursor For
                Select * from Mfgl11pro;
              Exec Sql Open Procursor;
              Exec Sql
                Fetch First from Procursor into
                   :Sproid,:Spronam,:Sprocat,:Sprosta;
              Dow Sqlcode=0;
     ** ¬ HANDLING ACTIVE/INACTIVE FOR PRODUCT STATUS  |
                If Sprosta = 'I';
                  Colourind = *On;
                Else ;
                  Colourind = *Off;
                Endif;
               If W_count=2 and Sprosta='A';
                Prorrn+=1;
                W_proid2(Prorrn) = %Int(Sproid);
                Write Mngspro;
              Elseif W_count = 3 and Sprosta='I';
                 Prorrn+=1;
                W_proid2(Prorrn) = %Int(Sproid);
                Write Mngspro;
              Elseif W_count=1;
                Prorrn+=1;
                W_proid2(Prorrn) = %Int(Sproid);
                Write Mngspro;
              Endif;
                If Prorrn>9999;
                  Leave;
                Endif;
                // Write Mngspro;
                Exec Sql
                Fetch Next from Procursor into
                   :Sproid,:Spronam,:Sprocat,:Sprosta;
              Enddo;
              Exec Sql Close Procursor;
              Endif;
            Endsr;

     ** ¬ DECLARING DISPLAY SUBFILE FOR MANAGE PRODUCT  |
            Begsr Prodspsfl;
              Prosfldsp = *On;
              Prosflctl = *On;
              Prosflend = *On;
              Dow Cancel = *Off;
                If Prorrn < 1;
                  Prosfldsp = *Off;
                Else;
                  Prosfldsp = *On;
                Endif;
                Dhcomp   = W_Company;
                Dhheader = '    Manage Product';
                Dprognam = Pgmname;
                Dhopt    = '2=Update   4=Delete   5=Display';
                Dfooter  = 'F5=Refresh   F6=Create   F7=Active/Inactive'+
                            '   F11=Reset   F12=Cancel' ;
                Write Header;
                Write Footer;
                Exfmt Mngcpro;
     ** ¬ HANDLE POSITION TO FOR MANAGE PRODUCT |
               If Spos <> *Zeros and Prorrn > 1
                  and SFPROCAT = *Blanks and SFPRONAM = *Blanks;
               Exec Sql
                Select Prid into :W_proid3 from MfgT03pro where Prid >= :Spos;
               If W_proid3 = *Zeros;
                Sprorrn = 1;
                Clear Spos;
              Else;
                Sprorrn = %lookup(W_proid3: W_proid2);
                Clear W_proid3;
                Clear Spos;
              Endif;
            Else;
              Sprorrn = Min_rrn;
              Clear Spos;
            Endif;





     ** ¬ F7 KEY HANDLING FOR MANAGE PRODUCT |
            If Actinact = *On;
              Actinact = *Off;
              W_count+=1;
              If W_count = 4;
                W_count = 1;
              Endif;
              Exsr  Proclrsfl;
              Exsr Proloadsfl;
            Endif;

     ** ¬ F11 KEY HANDLING FOR MANAGE PRODUCT |
            If Resete = *On;
              Clear Sfprocat;
              Clear Sfpronam;
              Exsr  Proclrsfl;
              Exsr Proloadsfl;
              Resete = *Off;
            Endif;


            Select;
     ** ¬ CREATE PRODUCT CALLING |
              When Crupd=*On;
                Crupd=*Off;
                Productcreate(W_Company:Pgmname:W_usrid);
                Exsr Proclrsfl;
                Exsr Proloadsfl;
              When Refresh = *On;
                 Refresh = *Off;
                 Clear Sopt;
                 Clear Derrmsg;
                 Optind = *Off;
                 CLear Sfprocat;
                 Clear Sfpronam;
                 Clear SPos;
                 W_count = 1;
                 Exsr Proclrsfl;
                 Exsr Proloadsfl;
                Other;
                  W_optcheck = *Off;
                  Readc Mngspro;
     ** ¬ INVALID OPTION HANDLING FOR MANAGE PRODUCT |
                  Dow Not %Eof();
                    Optind = *Off;
                    If Sopt <> *Blanks And Sopt <> '2' And Sopt <> '5'
                    and Sopt <> '4';
                      Optind     = *On;
                      W_optcheck = *On;
                    Endif;
                    If Sprosta = 'I';
                       Colourind = *On;
                    Else ;
                      Colourind = *Off;
                    Endif;
                    Sflnxtchgind = *On;
                    Update Mngspro;
                    Readc Mngspro;
                  Enddo;
                  IF W_optcheck = *On;
                    Derrmsg = 'Invalid Option';
                  Else;
                    Clear Derrmsg;
                    Readc Mngspro;
                    Clear W_optcheck1;
     ** ¬ 4TH OPTION HANDLING IF STATUS IS ACTIVE FOR MANAGE PRODUCT |
                    Dow Not %Eof();
                      Optind = *Off;
                      If Sopt = '4' and Sprosta = 'A'
                        or Sopt ='2' and Sprosta = 'I';
                        Optind      = *On;
                        W_optcheck1 = *On;

                      Endif;
                       If Sprosta = 'I';
                         Colourind = *On;
                       Else ;
                         Colourind = *Off;
                       Endif;
                      Sflnxtchgind = *On;
                      Update Mngspro;
                      Readc Mngspro;
                    Enddo;
                    If W_optcheck1 = *On;
                      Derrmsg = 'Record Cannot be Used For That Operation';
                  Else;
                    Readc Mngspro;
                    Dow Not %EOf();
     ** ¬ UPDATE OPTION CALLING FOR MANAGE PRODUCT |
                    If Sopt = '2';
                      W_proid1 = %Int(Sproid);
                      Productupdate(W_Company:Pgmname:W_proid1);
                      Clear Sopt;
                      Update Mngspro;
                      Exsr Proclrsfl;
                      Exsr Proloadsfl;
     ** ¬ DISPLAY OPTION CALLING FOR MANAGE PRODUCT |
                    ElseIf Sopt = '5';
                      W_proid1 = %Int(Sproid);
                      Productdsply(W_Company:Pgmname:W_proid1);
                      Clear Sopt;
                      Update Mngspro;
     ** ¬ DELETE OPTION CALLING FOR MANAGE PRODUCT |
                    Elseif Sopt = '4';
                      W_countdel+=1;
                      W_prodel(W_countdel) = %Int(Sproid);
                      Clear Sopt;
                      Update Mngspro;
                    Endif;
                     If W_countdel > 0;
                       Productdelete(W_Company:Pgmname:W_prodel:W_countdel);
                       Clear W_countdel;
                       Exsr Proclrsfl;
                       Exsr Proloadsfl;
                     Endif;

                    Readc Mngspro;
                  Enddo;
                Endif;
                Endif;
                Endsl;


                If ( (SFPRONAM <> *BLANKS or SFPROCAT <> *BLANKS)
                    and w_optcheck  = *OFF
                     and w_optcheck1 = *OFF );

              Exsr Proclrsfl;
              Exsr Proloadsfl;
            Endif;

              Enddo;
              Reset Cancel;
              Clear Sopt;
              Clear Derrmsg;
              Optind = *Off;
              CLear Sfprocat;
              Clear Sfpronam;
              Clear SPos;
            Endsr;
         End-proc MngProproc;

     ** ¬ CREATE OPTION HANDLING  |
         Dcl-proc Productcreate;
           Dcl-pi Productcreate ;
             W_Company Char(32);
             Pgmname   Char(10);
             W_usrid   Packed(6);
           End-pi;
           Exsr Clearind;
           Dow Cancel=*Off;
             Dprocrt  = W_usrid;
             Proundpr = *On;
             Prostatprnd = *On;
             Prostatund  = *Off;
             Proprnd     = *Off;
             Proptypund  = *On;
             Proprtypnd  = *Off;
             Dprostat    = 'A';
             Dhcomp   = W_Company;
             Dhheader = '    Create Product';
             Dprognam = Pgmname;
             Dfooter  = 'F4=Prompt   F5=Refresh   F6=Create   F12=Cancel' ;
             Write Header;
             Write Footer;
             Exsr Productautoincrement;
             Exfmt Procrud;

     ** ¬ PROMPT OPTION FOR PRODUCT TYPE  |
             If Prompt=*On and FLd='DPROTYP';
               Prompt=*Off;
               Dow Cancel = *Off;
                 Exfmt Protwin;
                 If Dwcho='1';
                   Dprotyp = 'COMPLETED';
                   Leave;
                 Elseif Dwcho = '2';
                   Dprotyp = 'RAW MATERIAL';
                   Leave;
                 Elseif Dwcho = '3';
                   Dprotyp = 'SEMI COMPLETED';
                   Leave;
                 Else;
                   Dwerrmsg  = 'Invalid Choice';
                   Prowinrev = *On;
                 Endif;
               Enddo;
               Cancel = *Off;
               Clear Prowinrev;
               Clear Dwerrmsg;
               Clear Dwcho;
             Endif;
     ** ¬ PROMPT OPTION FOR PRODUCT LOCATION |
             If Prompt = *On and Fld = 'DPROLOC';
               Prompt = *Off;
               Exsr Prolocsflclr;
               Exsr Prolocsflload;
               Exsr Prolocsfldsp;

             Endif;

     ** ¬ REFRESH OPTION |
             If Refresh = *On;
               Refresh = *Off;
               Exsr Clearfield;
               Exsr Clearind;
             Else;
     ** ¬ BLANK FIELD CHECK |
             If Dpronam=*Blanks;
               Pronmrp=*On;
               Derrmsg='Product Name Cannot Be Blank';
             Elseif Dprotyp = *Blanks;
               Exsr Clearind;
               Protyprp = *On;
               Derrmsg='Product Type Cannot Be Blank';
             Elseif Dproloc = *Zeros;
               Exsr Clearind;
               Prolocrp = *On;
               Derrmsg  = 'Product Location Id Cannot Be Blank';
             Elseif Dprodes = *Blanks;
               Exsr Clearind;
               Prodesrp = *On;
               Derrmsg  = 'Product Description Cannot Be Blank';
             Elseif  %len(%trim(Dprodes)) < 15;
               Exsr Clearind;
               Prodesrp = *On;
               Derrmsg  = 'Product Description Is Not Valid';
             Else;
               Exsr Clearind;
     ** ¬ F6 KEY HANDLING FOR CREATE PRODUCT |
               If Crupd = *On;
                 Crupd = *Off;
     ** ¬ INSERT THE RECORD FOR CREATE PRODUCT |
               Exec Sql
                 Insert into MfgT03Pro
                 Values (:Dproid, :Dpronam, :Dprotyp, :Dprostat, :Dprocrt
                         ,:Dproloc, :Dprodes);
     ** ¬ INSERT THE RECORD FOR STOCK PF |
                Exec Sql
                 Insert into Mfgt09Sto
                   Values (:Dproid, :Dpronam, :Dprotyp,0);
                 Derrmsg =  'Record Added Successfully';
                 Leave;
                Endif;
             Endif;
            Endif;
           Enddo;
           IF Cancel = *On;
             Clear Derrmsg;
           Endif;
           Reset Cancel;
           Exsr Clearfield;
     ** ¬ AUTOINCREMENT OF PRODUCT ID HANDLING|
           Begsr Productautoincrement;
            Exec Sql
              Select Max(Prid) into :W_proid from Mfgt03pro;
              If W_proid = *Zeros;
                W_proid = 300000;
              Endif;
              Dproid = W_proid+1;
           Endsr;
     ** ¬ TURNING OFF REVERSE IMAGE INDICATOR|
           Begsr Clearind;
             Pronmrp   = *Off;
             Protyprp  = *Off;
             Prostatrp = *Off;
             Prolocrp  = *Off;
             Prodesrp  = *Off;
             Clear Derrmsg;
           Endsr;
     ** ¬ CLEARING FIELD |
           Begsr Clearfield;
             Clear Dpronam;
             Clear Dprotyp;
             Clear Dproloc;
             Clear Dprodes;
           Endsr;
            Begsr Prolocsflclr;
                 Wlocsflclr = *On;
                 Wlocrrn    = 0;
                 Write Loccwin;
                 Wlocsflclr = *Off;
               Endsr;


               Begsr Prolocsflload;
                 Exec Sql
                   Declare Wloccursor Scroll Cursor For
                  Select Loid,Lona from Mfg07lo where Lostat='A';
                 Exec Sql Open Wloccursor;
                 Exec Sql
                    Fetch First  from WLoccursor into :Wslocid ,:Wslocnam;
                 Dow Sqlcode=0;
                   Wlocrrn+=1;
                   If Wlocrrn > 9999;
                    Leave;
                   Endif;
                   Write Locswin;
                   Exec Sql
                    Fetch Next  from WLoccursor into :Wslocid ,:Wslocnam;
                 Enddo;
                 Exec Sql Close Wloccursor;
               Endsr;

              Begsr Prolocsfldsp;
                 Wlocsfldsp = *On;
                 Wlocsflctl = *On;
                 Wlocsflend = *On;
                 Dow Cancel = *Off;
                   If Wlocrrn < 1;
                     Wlocsfldsp = *Off;
                   Endif;
                   Write Wfooter;
                   Exfmt Loccwin;
                   Readc Locswin;
                   If Swopt = 'X';
                     Dproloc = Wslocid;
                     Clear  Swopt;
                     Update Locswin;
                     Leave;
                   Endif;
                 Enddo;
                 Reset Cancel;
                 Wlocsfldsp = *Off;
                 Wlocsflctl = *Off;
               Endsr;
         End-proc Productcreate;

     ** ¬ DISPLAY OPTION HANDLING  |
         Dcl-proc Productdsply;
          Dcl-pi Productdsply ;
            W_Company Char(32);
            Pgmname   Char(10);
            W_proid1  Packed(6);
          end-pi;
          Proundpr    = *Off;
          Prostatprnd = *On;
          Prostatund  = *Off;
          Proprnd     = *On;
          Proptypund  = *off;
          Proprtypnd  = *On;
     ** ¬ FETCHING A RECORD FOR PARTICULAR ID  |
          Exec Sql
            Select * into :Dproid,
            :Dpronam,:Dprotyp,:Dprostat,:Dprocrt,:Dproloc,:Dprodes
             from MFGT03PRO where PRID = :W_proid1;
          Dow Cancel = *Off;
            Dhcomp   = W_Company;
            Dhheader = '    Display Product';
            Dprognam = Pgmname;
            Dfooter  = 'F12=Cancel' ;
            Write Header;
            Write Footer;
            Exfmt Procrud;
          Enddo;
          Reset Cancel;
         end-proc Productdsply;

     ** ¬ UPDATE OPTION HANDLING  |
         Dcl-proc Productupdate;
          Dcl-pi Productupdate;
            W_Company Char(32);
            Pgmname   Char(10);
            W_proid1  Packed(6);
          end-pi;
          Proundpr    = *On;
          Prostatprnd = *Off;
          Prostatund  = *On;
          Proprnd     = *Off;
          Proptypund  = *Off;
          Proprtypnd  = *On;
          Exec Sql
            Select * into :Dproid,
            :Dpronam,:Dprotyp,:Dprostat,:Dprocrt,:Dproloc,:Dprodes
             from MFGT03PRO where PRID = :W_proid1;
          Exsr Clearind;
          Dow Cancel = *Off;
            Dhcomp   = W_Company;
            Dhheader = '    Update Product';
            Dprognam = Pgmname;
            Dfooter  = 'F4=Prompt   F5=Refresh   F6=Update   F12=Cancel' ;
            Write Header;
            Write Footer;
            Exfmt Procrud;
     ** ¬ PROMPT FIELD FOR STATUS TYPE|
            If Prompt = *On and Fld= 'DPROSTAT';
              Prompt = *Off;
              Dow Cancel = *Off;
                Dwhead = 'Product Status';
                Exfmt STAWIN;
                If Wchoice = '1';
                  Dprostat = 'A';
                  Leave;
                Elseif Wchoice ='2';
                  Exec Sql
                   Select Count(*) into :W_proactive
                   from Mfgt05wo where
                    Woprd = :W_proid1 and Wosta='A' ;
                  If W_proactive>0;
                    Werrmsg='Product Cannot Be Inactive';
                  Else;
                    Dprostat = 'I';
                    Exec Sql
                     Update Mfgt04bom
                     set Bosat = 'I'
                     where Boprd=:W_proid1;
                     Leave;
                  Endif;
                Elseif Wchoice = *Blanks;
                  Clear Werrmsg;
                   Clear Wchorv;
                Else;
                    Werrmsg = 'Invalid Choice';
                    Wchorv = *On;
                Endif;
              Enddo;
              Reset Cancel;
               Clear Werrmsg;
               Clear Wchorv;
               Clear Wchoice;
            Endif;

            If Prompt = *On and Fld = 'DPROLOC';
              Prompt = *Off;
              Exsr Prolocsflclr;
              Exsr Prolocsflload;
              Exsr Prolocsfldsp;
            Endif;
            If Refresh = *On;
              Refresh = *On;
              Exec Sql
              Select * into :Dproid,
              :Dpronam,:Dprotyp,:Dprostat,:Dprocrt,:Dproloc,:Dprodes
              from MFGT03PRO where PRID = :W_proid1;
            Endif;

            If Dpronam = *Blanks;
              Pronmrp = *On;
              Derrmsg = 'Product Name Cannot be Blank';
            Elseif Dproloc = *Zeros;
              Exsr Clearind;
              Pronmrp = *On;
              Derrmsg = 'Product Location Cannot Be Blank';
            Elseif Dprodes = *Blanks;
              Exsr Clearind;
              Prodesrp = *On;
              Derrmsg  = 'Product Description Cannot Be Blank';
            Elseif %len(%trim(Dpronam)) <= 3;
              Exsr Clearind;
              Pronmrp = *On;
              Derrmsg  = 'Product Name Is Not Valid';
            Elseif %len(%trim(Dprodes)) < 15;
              Exsr Clearind;
              Prodesrp = *On;
              Derrmsg  = 'Product Description Is Not Valid';
            Else;
              Exsr Clearind;
              If Crupd = *On;
                Exec Sql
                 Update Mfgt03pro
                 Set Prnam = :Dpronam,
                     Prloc = :Dproloc,
                     Prodes= :Dprodes,
                     Prsta = :Dprostat
                  where Prid = :Dproid;
                 Derrmsg = 'Record Updated Successfully';
                Leave;
              Endif;
            Endif;
          Enddo;
          If Cancel = *On;
           Clear Derrmsg;
          Endif;
          Reset Cancel;
          Exsr Clearfield;
          Begsr Clearind;
             Pronmrp   = *Off;
             Protyprp  = *Off;
             Prostatrp = *Off;
             Prolocrp  = *Off;
             Prodesrp  = *Off;
             Clear Derrmsg;
           Endsr;

           Begsr Prolocsflclr;
             Wlocsflclr = *On;
             Wlocrrn    = 0;
             Write Loccwin;
             Wlocsflclr = *Off;
           Endsr;

           Begsr Prolocsflload;
                 Exec Sql
                   Declare Wloccursor1 Scroll Cursor For
                  Select Loid,Lona from Mfg07lo where Lostat='A';
                 Exec Sql Open Wloccursor1;
                 Exec Sql
                    Fetch First  from WLoccursor1 into :Wslocid ,:Wslocnam;
                 Dow Sqlcode=0;
                   Wlocrrn+=1;
                   If Wlocrrn > 9999;
                    Leave;
                   Endif;
                   Write Locswin;
                   Exec Sql
                    Fetch Next  from WLoccursor1 into :Wslocid ,:Wslocnam;
                 Enddo;
                 Exec Sql Close Wloccursor1;
           Endsr;

           Begsr Prolocsfldsp;
             Wlocsfldsp = *On;
             Wlocsflctl = *On;
             Wlocsflend = *On;
            Dow Cancel = *Off;
             If Wlocrrn < 1;
               Wlocsfldsp = *Off;
             Endif;
             Write Wfooter;
             Exfmt Loccwin;
             Readc Locswin;
             If Swopt = 'X';
               Dproloc = Wslocid;
               Clear  Swopt;
               Update Locswin;
               Leave;
             Endif;
           Enddo;
           Reset Cancel;
           Wlocsfldsp = *Off;
           Wlocsflctl = *Off;
        Endsr;

          Begsr Clearfield;
             Clear Dpronam;
             Clear Dprotyp;
             Clear Dproloc;
             Clear Dprodes;
             Clear Dprostat;
           Endsr;

         End-proc Productupdate;


         Dcl-proc Productdelete ;
          Dcl-pi Productdelete;
            W_Company Char(32);
            Pgmname   Char(10);
            W_prodel  Packed(6) Dim(9999);
            W_countdel Packed(6);
          End-pi;
          Exsr  Prodelsflclr;
          Exsr  Prodelsflload;
          Exsr  Prodelsfldsply;

          Begsr Prodelsflclr;
            Prodsflclr = *On;
            Prodrrn    = 0;
            W_countdel1 = 0;
            Write Prodcdel;
            Prodsflclr = *Off;
          Endsr;

          Begsr Prodelsflload;
           Dow W_countdel1 < W_countdel;
            W_prodelid = W_prodel(W_countdel1+1);
             Exec Sql
             Select Prid,Prnam,Prcat into :DDELPROID,
             :DDEPRONAM, :DDELPROCAT from Mfgt03pro
             where Prid = :W_prodelid;
             Prodrrn+=1;
             W_countdel1+=1;
             Write Prodsdel;
           Enddo;
          Endsr;

          Begsr Prodelsfldsply;
            Prodsfldsp = *On;
            Prodsflctl = *on;
            Prodsflend = *On;
            Dow Cancel = *Off;
              If Prodrrn < 1;
                Prodsfldsp = *Off;
              Endif;
              Dhcomp   = W_Company;
              Dhheader = '    Delete Product';
              Dprognam = Pgmname;
              Dfooter  = 'F12=Cancel' ;
              Write Header;
              Write Footer;
              Exfmt Prodcdel;
            Enddo;
            Reset Cancel;

          Endsr ;

         End-proc Productdelete;




