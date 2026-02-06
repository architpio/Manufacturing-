     *-------------------------------------------------------------        *
     * Program Name  : MFGADRPG6                                           *
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
         Dcl-f Mfg01dsp Workstn Sfile(Mngsloc:Locrrn) Indds(Indds)
                                                      Infds(Subinfo);

     ** ¬INDICATOR DATA STRUCTURE DECLARATION |
         Dcl-ds Indds;
           Prompt       Ind Pos(04);
           Refresh      Ind Pos(05);
           Crupd        Ind Pos(06);
           Actinact     Ind Pos(07);
           Cancel       Ind Pos(12);
           Locsfldsp    Ind Pos(54);
           Locsflctl    Ind Pos(55);
           Locsflclr    Ind Pos(56);
           Locsflend    Ind Pos(57);
           Locnamrp     Ind Pos(15);
           Locpinrp     Ind Pos(16);
           Locstarp     Ind Pos(17);
           Locstatrp    Ind Pos(18);
           Prstatnd     Ind Pos(19);
           Lostaund     Ind Pos(25);
           Prstand      Ind Pos(20);
           Locund       Ind Pos(26);
           Optind       Ind Pos(28);
           Sflnxtchgind Ind Pos(30);
         End-ds;

         Dcl-ds Subinfo;
           Min_RRN Int(5:0) Pos(378);
         End-ds;

     ** ¬ PROCEDURE DECLARATION FOR STATE SUBFILE |
         Dcl-pr Stateproc;
           W_Company Char(32);
           Pgmname   Char(10);
           W_state   Char(20);
         End-pr;

     ** ¬ PROCEDURE DECLARATION FOR CREATE LOCATION |
         Dcl-pr Createloc;
            W_Company char(32);
            Pgmname   char(10);
         End-pr;

     ** ¬ PROCEDURE DECLARATION FOR DISPLAY LOCATION |
         Dcl-pr Dsplyloc;
            W_Company char(32);
            Pgmname   char(10);
            W_locid   Packed(6);
         End-pr;

     ** ¬ PROCEDURE DECLARATION FOR UPDATE LOCATION |
         Dcl-pr Updateloc;
            W_Company char(32);
            Pgmname   char(10);
            W_locid   Packed(6);
         End-pr;

     ** ¬ WORK VARIABLE DECLARATION |
         Dcl-s Locrrn Packed(4:0);
         Dcl-s W_Loid     Packed(6);
         Dcl-s W_state    char(20);
         Dcl-s W_locid    Packed(6);
         Dcl-s W_optcheck Ind;
         Dcl-s W_locid2   Packed(6);
         Dcl-s W_locid1   Packed(6) Dim(9999);
         Dcl-s W_loccount Packed(6);
         Dcl-s W_count    Packed(5) Inz(1);
         Dcl-s W_pos1     Packed(2);
         Dcl-s W_name     VarChar(60);

         Dcl-proc Locproc Export;
            Dcl-pi Locproc ;
              W_Company char(32);
              Pgmname   char(10);
            end-pi;
           Exec SQL SET Option Commit = *None;
           Slocrrn=1;
           Exsr Locclrsfl;
           Exsr Locloadsfl;
           Exsr Locdspsfl;

     ** ¬ SUBROUTINE FOR CLEARING LOCATION SUBROUTINE|
           Begsr Locclrsfl;
             Locsflclr=*On;
             Locrrn   = 0;
             Write Mngcloc;
             Locsflclr = *Off;
             Clear W_locid1;
           Endsr;

     ** ¬ SUBROUTINE FOR DISPLAY LOCATION SUBROUTINE|
          Begsr Locloadsfl;
            Exec Sql
              Declare Loccursor Scroll Cursor For
              Select * from MFGV10LO;
            Exec Sql Open Loccursor;


     ** ¬ CURSOR DECLARATION FOR FETCHING LOCATION DETAIL|
            Exec Sql
              Fetch First  from Loccursor into :Slocid,:SLOCNAM,:SLOCSTA;
            Dow Sqlcode=0;

              If W_count = 2 And Slocsta = 'A';
                Locrrn+=1;
                Write Mngsloc;
                W_locid1(Locrrn) = %Int(Slocid);
              Elseif W_count = 3 And Slocsta = 'I';
                Locrrn+=1;
                Write Mngsloc;
                W_locid1(Locrrn) = %Int(Slocid);
              Elseif W_count = 1;
                Locrrn+=1;
                Write Mngsloc;
                W_locid1(Locrrn) = %Int(Slocid);
              Endif;

              IF Locrrn > 9999;
                Leave;
              Endif;
              Exec Sql
               Fetch Next from Loccursor into
               :Slocid,:SLOCNAM,:SLOCSTA;
            Enddo;
            Exec Sql Close Loccursor;
          Endsr;

     ** ¬ SUBROUTINE FOR DISPALY LOCATION SUBROUTINE|
          Begsr Locdspsfl;
            Locsfldsp = *On;
            Locsflctl = *On;
            Locsflend = *On;


            Dow Cancel = *Off;
               If Locrrn < 1;
                 Locsfldsp = *Off;
               Else;
                 Locsfldsp = *On;
               Endif;
              Dhcomp   = W_Company;
              Dhheader = '    Manage Location';
              Dprognam = Pgmname;
              Dhopt    = '2=Update   5=Display';
              Dfooter  = 'F5=Refresh   F6=Create   F7=Active/Inactive' +
                          '  F12=Cancel' ;
              Write Header;
              Write Footer;
              Exfmt Mngcloc;

     ** ¬ POSITION TO FUNCTIONALITY|
              IF Spos <> *Zeros And Locrrn > 1;
                Exec Sql
                Select Loid into :W_locid2 from Mfg07lo where Loid>=:Spos;
                If W_locid2 = *Zeros;
                  Slocrrn = 1;
                  Clear Spos;
                Else;
                  Slocrrn = %lookup(W_locid2: W_locid1);
                  Clear W_locid2;
                  Clear Spos;
                Endif;
              Else;
                Slocrrn=Min_rrn;
              Endif;

     ** ¬ F7 KEY HANDLING FOR ACTIVE AND INACTIVE RECORD|
              If Actinact = *On;
                Actinact = *Off;
                W_count+=1;
                If W_count = 4;
                  W_count = 1;
                Endif;
                Exsr Locclrsfl;
                Exsr Locloadsfl;
              Endif;
              Select;

     ** ¬CREATE LOCATION HANDLING|
                When Crupd = *On;
                   Crupd = *Off;
                   Createloc(W_Company: Pgmname);
                   Exsr Locclrsfl;
                   Exsr Locloadsfl;
                When Refresh = *On;
                  Refresh = *Off;
                  Clear Derrmsg;
                  Clear Sopt;
                  Clear Optind;
                  W_count = 1;
                  Exsr Locclrsfl;
                  Exsr Locloadsfl;
                  Clear Spos;
                Other;
                  Readc Mngsloc;
                  W_optcheck = *Off;

     ** ¬INVALID OPTION HANDLING|
                  Dow Not %Eof();
                    Optind=*Off;
                    If Sopt <> *Blanks And Sopt <> '2' And Sopt <> '5';
                      Optind = *On;
                      W_optcheck = *On;
                    Endif;
                    Sflnxtchgind = *On;
                    Update Mngsloc;
                    Readc Mngsloc;
                  Enddo;
                   If W_optcheck = *On;
                     Derrmsg = 'Invalid Option';
                   Else;
                     Clear Derrmsg;
                     Readc Mngsloc;
                     Dow Not %Eof();

     ** ¬DISPLAY LOCATION OPTION HANDLING|
                    IF Sopt = '5';
                      W_locid = %Int(Slocid);
                      Dsplyloc(W_Company: Pgmname: W_locid);
                      Sopt = *Blanks;
                      Update Mngsloc;

     ** ¬UPDATE LOCATION OPTION HANDLING|
                    Elseif Sopt = '2';
                      W_locid = %Int(Slocid);
                      Updateloc(W_Company: Pgmname: W_locid);
                      Sopt = *Blanks;
                      Update Mngsloc;
                    Endif;
                    Readc Mngsloc;
                  Enddo;
                  Exsr Locclrsfl;
                  Exsr Locloadsfl;
                Endif;
              Endsl;

            Enddo;
            Reset Cancel;
            W_count=1;
            Clear Derrmsg;
            Clear Sopt;
            Clear Optind;
            Clear Spos;
          Endsr;


         End-proc Locproc;

     ** ¬CREATE  LOCATION FUNCTIONALITY HANDLING PROCEDURE|
         Dcl-proc Createloc Export;
          Dcl-pi Createloc ;
             W_Company char(32);
             Pgmname   char(10);
          End-pi;
            Dow Cancel=*Off;
              Dhcomp   = W_Company;
              Dhheader = '    Create Location';
              Dprognam = Pgmname;
              Dfooter  = 'F4=Prompt   F5=Refresh   F6=Create   F12=Cancel' ;
              Locund   = *On;
              Prstatnd = *Off;
              Prstand  = *On;
              Lostaund = *Off;
              Dlocstat = 'A';
              Write Header;
              Write Footer;
              Exsr Locautoincrement;
              Exfmt Loccrud;

     ** ¬PROMPT FOR STATE FIELD|
              If Prompt = *On and Fld = 'DLOCSTA';
                Prompt = *Off;
                Stateproc(W_Company: Pgmname: W_state);
                DLOCSTA = W_state;
              Endif;

     ** ¬REFRESH FUNCTIONALITY |
              If Refresh = *On;
                Exsr Clrlocfld;
                Exsr ClearLocind;
                Refresh = *Off;
              Else;
                If DLOCNAM = *BLanks;
                Locnamrp = *On;
                Derrmsg = 'Location Name Field Cannot be Blank';
                Elseif DLOCPIN = *Zeros;
                  Exsr ClearLocind;
                  Exsr Nameautocorrect;
                  Locpinrp = *On;
                  Derrmsg  = 'Location Pin Field Cannot be Blank';
                Elseif DLOCSTA = *Blanks;
                  Exsr ClearLocind;
                  Locstarp = *On;
                  Derrmsg  = 'Location State Field cannot be Blank';
                Elseif %len(%trim(Dlocnam)) < 15;
                Exsr ClearLocind;
                Locnamrp=*On;
                Derrmsg='Location Name Field Is Not Valid';
              Elseif Dlocpin<100000;
                Exsr ClearLocind;
                Locpinrp=*On;
                Derrmsg='Location Pin Field Is Not Valid';
              Else;
                 Exsr ClearLocind;
                 If Crupd=*On;
                   Crupd=*Off;
                   Exec Sql
                    Insert into Mfg07Lo
                    Values(:Dlocid,:Dlocnam,:Dlocstat,:Dlocpin,:Dlocsta);
                    Dsply Sqlcode;
                    Leave;
                    Derrmsg='Record Added Successfully';
                 Endif;
              Endif;
               ENdif;
            Enddo;
            Reset Cancel;
            Exsr Clrlocfld;
            Exsr Clearlocind;
             Locund   = *Off;
               Prstatnd = *On;
              Prstand  = *On;
              Lostaund = *Off;
            Begsr ClearLocind;
              Locnamrp=*Off;
              Locpinrp=*Off;
              Locstarp=*Off;
              Locstatrp=*Off;
              Clear Derrmsg;
            Endsr;
            Begsr Clrlocfld;
              Clear Dlocnam;
              Clear Dlocpin;
              Clear Dlocsta;
            Endsr;
            Begsr Locautoincrement;
              Exec Sql
               Select Max(Loid) into :W_loid from Mfgv10lo;
               DLOCID=W_loid+1;
            Endsr;

            Begsr Nameautocorrect;
              W_pos1=1;
               Dou W_pos1=0;
                 Dlocnam=%trim(Dlocnam);
                 W_pos1= %scan(' ':Dlocnam);
                 W_name=W_name+' '+%subst(Dlocnam:1:W_Pos1-1);
                 Dlocnam=%subst(Dlocnam:W_pos1+1);
                 If %len(%trim(Dlocnam))=0;
                   leave;
                 endif;
              enddo;
              Dlocnam=%trim(W_name);
              Clear W_name;
            Endsr;
         end-proc Createloc;


         dcl-proc Dsplyloc;
          dcl-pi Dsplyloc ;
            W_Company char(32);
            Pgmname   char(10);
            W_locid   Packed(6);
          end-pi;
          Dow Cancel=*Off;
              Dhcomp   = W_Company;
              Dhheader = '    Display Location';
              Dprognam = Pgmname;
              Dfooter  = 'F12=Cancel';
              Locund   = *Off;
              Prstatnd = *On;
              Prstand  = *On;
              Lostaund = *Off;
              Write Header;
              Write Footer;
              Exec Sql
              Select * into :Dlocid,:Dlocnam,:Dlocstat,:Dlocpin,:Dlocsta
              from MFg07lo where Loid=:W_locid;
              Exfmt LOCCRUD;
          Enddo;
          Reset Cancel;
         end-proc Dsplyloc;


         Dcl-proc Updateloc Export;
          dcl-pi  Updateloc ;
             W_Company char(32);
             Pgmname   char(10);
             W_locid   Packed(6);
          end-pi;
          Exec Sql
              Select * into :Dlocid,:Dlocnam,:Dlocstat,:Dlocpin,:Dlocsta
              from MFg07lo where Loid=:W_locid;
            Dow Cancel=*Off;
              Dhcomp   = W_Company;
              Dhheader = '    Update Location';
              Dprognam = Pgmname;
              Dfooter  = 'F4=Prompt   F5=Refresh   F6=Update   F12=Cancel' ;
              Locund   = *On;
              Prstatnd = *Off;
              Prstand  = *Off;
              Lostaund = *On;
              Write Header;
              Write Footer;
              Exfmt Loccrud;
              If Prompt=*On and Fld='DLOCSTA';
                Prompt=*Off;
                Stateproc(W_Company:Pgmname:W_state);
                DLOCSTA=W_state;
              Endif;
              If Prompt=*On and Fld='DLOCSTAT';
                Prompt=*Off;
                Dow Cancel=*Off;
                  Dwhead='Location Status';
                  Exfmt Stawin;
                  IF Wchoice='1';
                    Dlocstat='A';
                    Leave;
                  Elseif Wchoice='2';
                    Exec Sql
                      Select Count(*) into :W_loccount from Mfgt023pro
                       where Prloc=:W_locid and Prsta='A';
                    IF W_loccount>1;
                       Werrmsg='Record Cannot Be Inactive ';
                    Else;
                      Dlocstat='I';
                      Leave;
                    Endif;
                  Else;
                    Werrmsg='Invalid Choice ';
                  Endif;
                Enddo;
                 Cancel=*Off;
                 Clear W_loccount;
                 Clear wchoice;
                 Clear werrmsg;
                Endif;
              If Refresh=*On;
                Exsr Clrlocfld;
                Exsr ClearLocind;
                Refresh=*Off;
                Exec Sql
               Select * into :Dlocid,:Dlocnam,:Dlocstat,:Dlocpin,:Dlocsta
               from MFg07lo where Loid=:W_locid;
              Else;
              If DLOCNAM=*BLanks;
                Locnamrp=*On;
                Derrmsg='Location Name Field Cannot be Blank';
              Elseif DLOCPIN=*Zeros;
                Exsr ClearLocind;
                Locpinrp=*On;
                Derrmsg='Location Pin Field Cannot be Blank';
              Elseif DLOCSTA=*Blanks;
                 Exsr ClearLocind;
                 Locstarp=*On;
                 Derrmsg='Location State Field cannot be Blank';
              Elseif %len(%trim(Dlocnam))<15;
                Exsr ClearLocind;
                Locnamrp=*On;
                Derrmsg='Location Name Field Is Not Valid';
              Elseif Dlocpin<100000;
                Exsr ClearLocind;
                Locpinrp=*On;
                Derrmsg='Location Pin Field Is Not Valid';
              Else;
                 Exsr ClearLocind;
                 Exsr Nameautocorrect;
                 If Crupd=*On;
                   Crupd=*Off;
                   Exec Sql
                    Update Mfg07Lo
                    set Loid    =:Dlocid,
                        Lona    =:Dlocnam,
                        Lostat  =:DLOCSTAT,
                        Lopin   =:Dlocpin,
                        Lostate =:Dlocsta
                       where Loid=:W_locid;
                    Dsply Sqlcode;
                    Leave;
                    Derrmsg='Record Updated Successfully';
                 Endif;
              Endif;
               ENdif;
            Enddo;
            Reset Cancel;
            Exsr Clrlocfld;
            Exsr Clearlocind;
             Locund   = *Off;
               Prstatnd = *On;
              Prstand  = *On;
              Lostaund = *Off;
            Begsr ClearLocind;
              Locnamrp=*Off;
              Locpinrp=*Off;
              Locstarp=*Off;
              Locstatrp=*Off;
              Clear Derrmsg;
            Endsr;
            Begsr Clrlocfld;
              Clear Dlocnam;
              Clear Dlocpin;
              Clear Dlocsta;
            Endsr;

            Begsr Nameautocorrect;
              W_pos1=1;
               Dou W_pos1=0;
                 Dlocnam=%trim(Dlocnam);
                 W_pos1= %scan(' ':Dlocnam);
                 W_name=W_name+' '+%subst(Dlocnam:1:W_Pos1-1);
                 Dlocnam=%subst(Dlocnam:W_pos1+1);
                 If %len(%trim(Dlocnam))=0;
                   leave;
                 endif;
              enddo;
              Dlocnam=%trim(W_name);
              Clear W_name;
            Endsr;

         end-proc Updateloc;



