     *-------------------------------------------------------------        *
     * Program Name  : MFGADRPG1                                           *
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
         Dcl-f Mfg01dsp Workstn sfile(MNGSEMP:EMPRRN)
          Sfile(EMpsdel:Delemprrn) Indds(Indds);

     ** ¬INDICATOR DATA STRUCTURE DECLARATION |
         Dcl-ds Indds;
           Exit          Ind Pos(03);
           Refresh       Ind Pos(05);
           Cancel        Ind Pos(12);
           Create        Ind Pos(06);
           Sfldsp        Ind Pos(50);
           Sflctl        Ind Pos(51);
           sflclr        Ind Pos(52);
           Sflend        Ind Pos(53);
           Optind        Ind Pos(28);
           Sflnxtchgind  Ind Pos(30);
           Subred        Ind Pos(31);
           Delsfldsp     Ind Pos(44);
           Delsflctl     Ind Pos(45);
           Delsflclr     Ind Pos(46);
           Delsflend     Ind Pos(47);
         End-ds;

     ** ¬ PROCEDURE DECLARATION FOR CREATE AND UPDATE EMPLOYEE SCREEN |
         Dcl-pr Empcrudproc;
           W_Company    Char    (32);
           Pgmname      Char    (10);
           W_operation  Char    (1);
           W_emptyp     Varchar (1);
           W_empid      Packed  (6) Options(*Nopass);
         End-pr;

     ** ¬ PROCEDURE DECLARATION FOR DISPLAY EMPLOYEE SCREEN |
         Dcl-pr Empdspsfl;
           W_Company Char   (32);
           Pgmname   Char   (10);
           W_emptyp  Varchar(1);
         End-pr;

     ** ¬ PROCEDURE DECLARATION FOR DELETE EMPLOYEE SCREEN |
         Dcl-pr Delempproc;
           W_Company Char  (32);
           Pgmname   Char  (10);
           Deleteid  Packed(6) Dim(9999);
           Count     Packed(6);
         End-pr;

     ** ¬ WORRK VARIABLE DECLARATION  |
         Dcl-s Emprrn      Packed(4:0);
         Dcl-s Delemprrn   Packed(4:0);
         Dcl-s W_operation Char(1);
         Dcl-s W_empid     Packed(6:0);
         Dcl-s W_optcheck  Ind;
         Dcl-s W_id packed(6) dim(9999);
         Dcl-s W_id1 Packed(6);
         Dcl-s W_optcheck1 Ind;
         Dcl-s Count Packed(6);
         Dcl-s Count1 Packed(6);
         Dcl-s Deleteid Packed(6) Dim(9999);


         Dcl-Proc Adempproc Export;
           Dcl-pi Adempproc;
             W_Company char(32);
             Pgmname   char(10);
             W_emptyp  Varchar(1);
           End-pi;

           Scrrn=1;
           Empclrsfl();
           Emploadsfl();
           Empdspsfl( W_Company: Pgmname: W_emptyp);

         End-proc Adempproc;

     ** ¬ MANAGE EMPLOYEE CLEAR SUBFILE SUBROUTINE|
         Dcl-proc Empclrsfl;
           Dcl-pi Empclrsfl;
           End-pi;
           Sflclr = *On;
           Emprrn = 0;
           Write Mngcemp;
           Sflclr = *Off;
           Clear W_id;
         End-proc Empclrsfl;

     ** ¬ MANAGE EMPLOYEE LOAD SUBFILE PROCEDURE|
         Dcl-proc Emploadsfl;
           Dcl-pi Emploadsfl;
           End-pi;

     ** ¬ DECLARE CURSOR FOR FETCHING EMPLOYEE DETAIL|
           Exec Sql
             Declare Empcursor Scroll Cursor For
             Select Empid,Empnam,Emptyp,Empsta from MFGT02EMP
             Where Empid>100000 order by Empsta;
           Exec Sql Open Empcursor;


     ** ¬ FETCHING FIRST REOCRD FROM CURSOR|
           Exec Sql
             Fetch First from Empcursor into :Sempid,:Sempnam,:Sempcat,:Sempsta;

           Dow Sqlcode=0;

             If Sempcat = 'P';
               Sempcat = 'Product Manager';
             Elseif Sempcat = 'B';
               Sempcat = 'Bom Planner';
             Elseif Sempcat = 'W';
               Sempcat = 'Production Supervisor';
             Else;
               Sempcat = 'Inventory Controller';
             Endif;

             If Sempsta = 'A';
               Sempsta = 'Active';
             Else;
               Sempsta = 'Inactive';
             Endif;
             Emprrn       = Emprrn+1;
             W_id(Emprrn) = Sempid;


     ** ¬GIVING COLOUR INDICATOR ON THE BASIS OF STATUS|
             IF Sempsta = 'Active';
              Subred = *OFf;
             ELse;
               Subred = *On;
             Endif;
             If Emprrn > 9999;
               Leave;
             Endif;
             Write Mngsemp;

     ** ¬ FETCHING NEXT REOCRD FROM CURSOR|
             Exec Sql
               Fetch Next from Empcursor into :Sempid,
                 :Sempnam,:Sempcat,:Sempsta;
           Enddo;
           Exec Sql Close Empcursor;
         End-proc Emploadsfl;

     ** ¬ MANAGE EMPLOYEE DISPLAY SUBFILE PROCEDURE|
         Dcl-proc Empdspsfl;
           Dcl-pi Empdspsfl;
             W_Company Char   (32);
             Pgmname   Char   (10);
             W_emptyp  Varchar(1);
           End-pi;

           Sfldsp    = *On;
           Sflctl    = *On;
           Sflend    = *On;
           Subred    = *Off;

           Dow Cancel=*Off;
             If Emprrn < 1;
               Sfldsp = *Off;
             Endif;
             Dhcomp   = W_Company;
             Dhheader = '    Manage Employee';
             Dprognam = Pgmname;
             Dhopt    = '2=Update   4=Delete   5=Display';
             Dfooter  = 'F5=Refresh   F6=Create   F12=Cancel' ;
             Write Header;
             Write Footer;
             Exfmt Mngcemp;

     ** ¬ POSITION TO FUNCTIONALITY|
             If Spos <> *Zeros And Emprrn > 1;
              Exec Sql
                Select Empid into :W_id1 from Mfgt02emp where Empid >= :Spos ;
                If W_id1 = *Zeros;
                  Scrrn = 1;
                  Clear Spos;
                Else;
                  Scrrn = %lookup( W_id1: W_id);
                  Clear W_id1;
                  Clear Spos;
                Endif;
             Endif;

             Select;

     ** ¬ CREATE EMPLOYEE FUNCTIONALITY|
               When Create = *On;
                 Create      = *Off;
                 W_operation = 'C';
                 Empcrudproc( W_Company: Pgmname: W_operation: W_emptyp);
                 Empclrsfl();
                 Emploadsfl();
                 Clear W_operation;

     ** ¬ REFRESH  FUNCTIONALITY FOR MANAGE EMPLOYEE|
               When Refresh = *On;
                Clear W_operation;
                Clear Derrmsg;
                Optind  = *Off;
                Refresh = *Off;
                Clear Sopt;
                Clear Pos;
                Empclrsfl();
                Emploadsfl();

               Other;
                 W_optcheck=*Off;
                 Readc Mngsemp;
     ** ¬ INVALID OPTION FUNCTIONALITY FOR MANAGE EMPLOYEE|
                 Dow Not %Eof();
                   Optind = *Off;
                   If Sopt <> *Blanks And Sopt <> '2' And Sopt <> '5'
                   and Sopt <> '4';
                     Optind     = *On;
                     W_optcheck = *On;
                   Endif;
                   Sflnxtchgind = *On;
                   Update Mngsemp;
                   Readc Mngsemp;
                        //  If Sopt='5';
                        //   W_empid=Sempid;
                        //   W_operation='D';
                        //   Empcrudproc(W_Company:Pgmname:W_operation:W_empid);
                        //   Sopt=*Blanks;
                        //  Elseif Sopt='2';
                        //    W_empid=Sempid;
                        //    W_operation='U';
                       //    Empcrudproc(W_Company:Pgmname:W_operation:W_empid);
                       //    Sopt=*Blanks;
                       //  Else;
                        //    Derrmsg='Invalid Option';
                        //    Optind=*On;
                          //    W_optcheck='N';
                       //  Endif;
                        //  IF sopt<>*Blanks or optind=*On;
                         //  ENdif;

                 Enddo;
                 IF W_optcheck = *On;
                   Derrmsg = 'Invalid Option';
                 Else;
                   Clear Derrmsg;
                   Readc Mngsemp;
                   Clear W_optcheck1;
                   Dow Not %Eof();
                     Optind = *Off;
                     If Sopt = '4' And Sempsta = 'Active';
                       Optind      = *On;
                       W_optcheck1 = *On;
                     Endif;
                      //  IF sopt<>*Blanks or optind=*On;
                     Sflnxtchgind=*On;
                     //  ENdif;
                     Update Mngsemp;
                     Readc Mngsemp;
                   Enddo;

                   If W_optcheck1 = *On;
                     Derrmsg = 'Record Cannot be Deleted';
                   Else;
                     Readc Mngsemp;
                    Dow Not %Eof();
                      If Sopt = '5';
                       W_empid     = Sempid;
                       W_operation = 'D';
                       Empcrudproc(W_Company: Pgmname: W_operation:
                                   W_emptyp: W_empid);
                       Sopt        = *Blanks;
                       Clear W_operation;
     ** ¬ DELETE FUNCTIONALITY FOR MANAGE EMPLOYEE SCREEN|
                      Elseif Sopt = '2';
                       W_empid     = Sempid;
                       W_operation = 'U';
                       Empcrudproc(W_Company: Pgmname: W_operation:
                                   W_emptyp: W_empid);
                       Clear W_operation;
                       Sopt = *Blanks;
                      Elseif Sopt = '4';
                       Count+= 1;
                       Deleteid(count) = SEmpid;
                       Sopt            = *Blanks;
                      Endif;
                      If Count > 0;
                       Delempproc(W_Company: Pgmname: Deleteid: Count);
                     Endif;
                      Update Mngsemp;
                     Readc Mngsemp;
                   Enddo;
                   Empclrsfl();
                   Emploadsfl();
                   Endif;
              Endif;
            Endsl;
           Enddo;
           Reset Cancel;
           Clear Sopt;
           Clear Derrmsg;
           Clear Optind;
           Clear Pos;
         End-proc Empdspsfl;


     ** ¬ DELETE FUNCTIONALITY FOR MANAGE EMPLOYEE SCREEN|
         Dcl-proc Delempproc;
          Dcl-pi Delempproc;
            W_Company    Char  (32);
            Pgmname      Char  (10);
            Deleteid    Packed(6) Dim(9999);
            Count       Packed(6);
          end-pi;
           Exsr Delsflclear;
           Exsr Delsflload;
           Exsr Delsfldsply;

     ** ¬ SUBROUTINE FOR CLEARING DELETE EMPLOYEE SUBFILE|
           Begsr Delsflclear;
             Delsflclr = *On;
             Delemprrn = 0;
             Write Empcdel;
             Delsflclr = *Off;
           Endsr;

     ** ¬ SUBROUTINE FOR LOAD DELETE EMPLOYEE SUBFILE|
           Begsr Delsflload;
             Dow Delemprrn < Count;
               Count1=Deleteid(delemprrn+1);
               Exec Sql
                 Select Empid,Empnam,Emptyp into :SEDEMPID,:SDEMPNAM,:SDEMPCAT
                 from MFGt02emp where Empid = :Count1;
                Delemprrn+=1;
                Write Empsdel;
             Enddo;
           Endsr;

     ** ¬ SUBROUTINE FOR DISPLAY DELETE EMPLOYEE SUBFILE|
           Begsr Delsfldsply;
             Delsfldsp = *On;
             Delsflctl = *On;
             Delsflend = *On;
             If Delemprrn < 1;
               Delsfldsp = *Off;
             Endif;
             Dow Cancel = *Off;
               Dhcomp   = W_Company;
               Dhheader = '    Delete Employee';
               Dprognam = Pgmname;
               Dfooter  = 'F12=Cancel' ;
               Write Header;
               Write Footer;
               Exfmt Empcdel;
             Enddo;
             Clear Count;
             Reset Cancel;
           Endsr;
         End-proc Delempproc;



