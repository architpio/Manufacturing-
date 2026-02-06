     **free
         Ctl-opt Nomain;
         Dcl-f Mfg01dsp Workstn sfile(Staswin:StaRRN) Indds(Indds) ;
         Dcl-ds Indds;
           Cancel Ind Pos(12);
           Sfldsp Ind Pos(40);
           Sflctl Ind Pos(41);
           Sflclr Ind Pos(42);
           Sflend Ind Pos(43);
         End-ds;
         Dcl-pr Stadspsfl;
           W_Company Char(32);
           Pgmname   Char(10);
           W_state   char(20);
         End-pr;
         Dcl-s Starrn Packed(4:0);
         Dcl-proc Stateproc Export;
           Dcl-pi Stateproc;
             W_Company char(32);
             Pgmname   char(10);
             W_state    char(20);
           End-pi;
           Staclrsfl();
           Staloadsfl();
           Stadspsfl(W_Company:Pgmname:W_state);
         End-proc Stateproc;

         Dcl-proc Staclrsfl;
           Dcl-pi Staclrsfl;
           End-pi;
           Sflclr=*On;
           Starrn=0;
           Write STACWIN;
           Sflclr=*Off;
         End-proc Staclrsfl;

         Dcl-proc Staloadsfl;
           Dcl-pi Staloadsfl;
           End-pi;
           Exec Sql
             Declare Stacursor Scroll Cursor For
             Select Stname from MFG08ST;
           Exec Sql Open Stacursor;
           Exec Sql
             Fetch First from Stacursor into :Wsstate;
           Dow Sqlcode=0;
             Starrn=Starrn+1;
             If Starrn>9999;
               Leave;
             Endif;
             Write Staswin;
             Exec Sql
               Fetch Next from Stacursor into :Wsstate;
           Enddo;
           Exec Sql Close Stacursor;
         End-proc Staloadsfl;

         Dcl-proc Stadspsfl;
           Dcl-pi Stadspsfl;
             W_Company Char(32);
             Pgmname   Char(10);
             W_State   Char(20);
           End-pi;
           Sfldsp    = *On;
           Sflctl    = *On;
           Sflend    = *On;
           If Starrn<1;
             Sfldsp=*Off;
           Endif;
           Dow Cancel=*Off;
             Exfmt Stacwin;
             Readc Staswin;
             IF WSOPT<>*Blanks;
               W_State=WSstate;
               Wsopt=*Blanks;
               Leave;
             Endif;
           Enddo;
         End-proc Stadspsfl;


