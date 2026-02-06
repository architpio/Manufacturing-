     **free
         Ctl-opt Nomain;


         Dcl-f Mfg01dsp Workstn Sfile(Locswin:Wlocrrn)
                         Sfile(Mngspro:Prorrn)  Indds(Indds) Infds(Subinfo);


         Dcl-ds Indds;
           Exit          Ind Pos(03);
           Prompt        Ind Pos(04);
           Refresh       Ind Pos(05);
           Actinact      Ind Pos(07);
           Cancel        Ind Pos(12);
           Resete        Ind Pos(11);
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
           Prostatprnd   Ind Pos(24);
           Proprnd       Ind Pos(21);
           Prowinrev     Ind Pos(23);
           Prostatund    Ind Pos(25);
           Proundpr      Ind Pos(26);
           Wlocsfldsp    Ind Pos(50);
           Wlocsflctl    Ind Pos(51);
           Wlocsflclr    Ind Pos(52);
           Wlocsflend    Ind Pos(53);
         End-ds;

         Dcl-pr Productcreate ;
           W_Company Char(32);
           Pgmname   Char(10);
           W_usrid   Packed(6);
         End-pr;

         Dcl-pr Productdsply ;
           W_Company Char(32);
           Pgmname   Char(10);
           W_proid1   Packed(6);
         End-pr;

         Dcl-ds Subinfo;
           Min_RRN Int(5:0) Pos(378);
         End-ds;


         Dcl-s Prorrn   Packed(4:0);
         Dcl-s Wlocrrn  Packed(4:0);
         Dcl-s W_proid  Packed(6);
         Dcl-s W_proid1 Packed(6);
         Dcl-s W_proid2   Packed(6) Dim(9999);
         Dcl-s W_proid3 Packed(6);
         Dcl-s W_sqlstmt     varchar(500);
         Dcl-s W_pattern     varchar(50);
         Dcl-s W_pattern1     varchar(50);
         Dcl-s W_count Packed(1);

         Dcl-proc MngProproc Export;
            Dcl-pi MngProproc;
              W_Company Char(32);
              Pgmname   Char(10);
              W_emptyp  Varchar(1);
              W_usrid   Packed(6);
            End-pi;
            Exec SQL SET Option Commit = *None;
            Exsr Proclrsfl;
            Exsr Proloadsfl;
            Exsr Prodspsfl;

            Begsr Proclrsfl;
              Prosflclr = *On;
              Prorrn    = 0;
              Write Mngcpro;
              Prosflclr = *Off;
              Sprorrn = 1;
            Endsr;

            Begsr Proloadsfl;
              If SFPRONAM <> *Blanks and SFPROCAT = *Blanks;
                W_pattern='%' + %trim(SFPRONAM) + '%';
                W_sqlstmt = 'SELECT * FROM MFGL11PRO WHERE PRNAM LIKE ?';
                EXEC SQL
                     PREPARE Dynstmt FROM :W_sqlstmt;               
                EXEC SQL
                  DECLARE Procursor1 Scroll CURSOR FOR Dynstmt;
                 Exec Sql Open Procursor1 using :W_pattern;
                Exec Sql
                Fetch First from Procursor1 into
                   :Sproid,:Spronam,:Sprocat,:Sprosta;
                Dow Sqlcode=0;
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
                Fetch Next from Procursor1 into
                   :Sproid,:Spronam,:Sprocat,:Sprosta;
              Enddo;
              Exec Sql Close Procursor1;
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
              Else;
              Exec Sql
                Declare Procursor Scroll Cursor For
                Select * from Mfgl11pro;
              Exec Sql Open Procursor;
              Exec Sql
                Fetch First from Procursor into
                   :Sproid,:Spronam,:Sprocat,:Sprosta;
              Dow Sqlcode=0;
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

            Begsr Prodspsfl;
              Prosfldsp = *On;
              Prosflctl = *On;
              Prosflend = *On;
              Dow Cancel=*Off;
                If Prorrn<1;
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
               If Spos <> *Zeros and Prorrn >1 
                  and SFPROCAT = *Blanks and SFPRONAM=*Blanks;
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
            If SFPRONAM <> *Blanks or SFPROCAT <> *Blanks ;
              Exsr Proclrsfl;
              Exsr Proloadsfl;
            Endif;

            If Actinact= *On;
              Actinact = *Off;
                W_count+=1;
                If W_count = 4;
                  W_count = 1;
                Endif;
               Exsr  Proclrsfl;
              Exsr Proloadsfl;
           Endif;

            If Resete = *On;
              Clear Sfprocat;
              Clear Sfpronam;
              Exsr  Proclrsfl;
              Exsr Proloadsfl;
              Resete = *Off;
            Endif;
                Select;
                When Crupd=*On;
                  Crupd=*On;
                  Productcreate(W_Company:Pgmname:W_usrid);
                  Exsr Proclrsfl;
                  Exsr Proloadsfl;
                Other;
                  Readc Mngspro;
                  Dow Not %EOf();
                    If Sopt = '5';
                      W_proid1 = %Int(Sproid);
                      Productdsply(W_Company:Pgmname:W_proid1);
                      Clear Sopt;
                      Update Mngspro;
                    Endif;
                    Readc Mngspro;
                  Enddo;
                Endsl;

              Enddo;
              Reset Cancel;
            Endsr;
         End-proc MngProproc;


         Dcl-proc Productcreate;
           Dcl-pi Productcreate ;
             W_Company Char(32);
             Pgmname   Char(10);
             W_usrid   Packed(6);
           End-pi;
           Dow Cancel=*Off;
             Dprocrt  = W_usrid;
             Proundpr = *On;
             Prostatprnd = *On;
             Prostatund  = *Off;
             Proprnd     = *Off;
             Dprostat    = 'A';
             Dhcomp   = W_Company;
             Dhheader = '    Create Product';
             Dprognam = Pgmname;
             Dfooter  = 'F4=Prompt   F5=Refresh   F6=Create   F12=Cancel' ;
             Write Header;
             Write Footer;
             Exsr Productautoincrement;
             Exfmt Procrud;

             If Prompt=*On and FLd='DPROTYP';
               Prompt=*Off;
               Dow Cancel = *Off;
                 Exfmt Protwin;
                 If Dwcho='1';
                   Dprotyp = 'Completed';
                   Leave;
                 Elseif Dwcho = '2';
                   Dprotyp = 'Raw Material';
                   Leave;
                 Elseif Dwcho = '3';
                   Dprotyp = 'Semi Completed';
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
             If Prompt = *On and Fld = 'DPROLOC';
               Prompt = *Off;
               Exsr Prolocsflclr;
               Exsr Prolocsflload;
               Exsr Prolocsfldsp;

             Endif;

             If Refresh = *On;
               Refresh = *Off;
               Exsr Clearfield;
               Exsr Clearind;
             Else;
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
               If Crupd = *On;
                 Crupd = *Off;
               Exec Sql
                 Insert into MfgT03Pro
                 Values (:Dproid, :Dpronam, :Dprotyp, :Dprostat, :Dprocrt
                         ,:Dproloc, :Dprodes);
                Exec Sql
                 Insert into Mfgt09Sto
                   Values (:Dproid, :Dpronam, :Dprotyp,0);
                 Derrmsg =  'Record Added Successfully';
                 Leave;
                Endif;
             Endif;
            Endif;
           Enddo;
           Reset Cancel;
           Exsr Clearfield;
           Exsr Clearind;
           Begsr Productautoincrement;
            Exec Sql
              Select Max(Prid) into :W_proid from Mfgt03pro;
              If W_proid=*Zeros;
                W_proid=300000;
              Endif;
              Dproid=W_proid+1;
           Endsr;
           Begsr Clearind;
             Pronmrp   = *Off;
             Protyprp  = *Off;
             Prostatrp = *Off;
             Prolocrp  = *Off;
             Prodesrp  = *Off;
             Clear Derrmsg;
           Endsr;
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
          Exec Sql
            Select * into :Dproid,
            :Dpronam,:Dprotyp,:Dprostat,:Dprocrt,:Dproloc,:Dprodes
             from MFGT03PRO where PRID = :W_proid1;
          Dow Cancel = *Off;
            Dhcomp   = W_Company;
            Dhheader = '    Display Product';
            Dprognam = Pgmname;
            Dfooter  = 'F12=Cancel' ;
            Exfmt Procrud;
          Enddo;
          Reset Cancel;
         end-proc Productdsply;



