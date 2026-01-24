     **free
         Ctl-opt Nomain;
         Dcl-f Mfg01dsp Workstn sfile(MNGSEMP:EMPRRN) Indds(Indds);
         Dcl-ds Indds;
           Exit    Ind Pos(03);
           Refresh Ind Pos(05);
           Cancel  Ind Pos(12);
           Create    Ind Pos(06);
           Sfldsp  Ind Pos(50);
           Sflctl  Ind Pos(51);
           sflclr  Ind Pos(52);
           Sflend  Ind Pos(53);
         End-ds;
         Dcl-pr Empcrudproc;
             W_Company    Char  (32);
             Pgmname      Char  (10);
             W_operation  Char  (1);
             W_empid      Packed(6) options(*Nopass);
         End-pr;
         Dcl-pr Empdspsfl;
           W_Company Char(32);
           Pgmname   Char(10);
         End-pr;
         Dcl-s Emprrn      Packed(4:0);
         Dcl-s W_operation Char(1);
         Dcl-s W_empid     Packed(6:0);
         Dcl-Proc Adempproc Export;
           Dcl-pi Adempproc;
             W_Company char(32);
             Pgmname   char(10);
           End-pi;
           Empclrsfl();
           Emploadsfl();
           Empdspsfl(W_Company:Pgmname);
         End-proc Adempproc;

         Dcl-proc Empclrsfl;
           Dcl-pi Empclrsfl;
           End-pi;
           Sflclr=*On;
           Emprrn=0;
           Write Mngcemp;
           Sflclr=*Off;
         End-proc Empclrsfl;

         Dcl-proc Emploadsfl;
           Dcl-pi Emploadsfl;
           End-pi;
           Exec Sql
             Declare Empcursor Scroll Cursor For
             Select Empid,Empnam,Empcat,Empsta from MFGT02EMP order by EMPID;
           Exec Sql Open Empcursor;
           Exec Sql
             Fetch First from Empcursor into :Sempid,:Sempnam,:Sempcat,:Sempsta;
           Dow Sqlcode=0;
             Emprrn=Emprrn+1;
             If Emprrn>9999;
               Leave;
             Endif;
             Write Mngsemp;
             Exec Sql
               Fetch Next from Empcursor into :Sempid,
                 :Sempnam,:Sempcat,:Sempsta;
           Enddo;
           Exec Sql Close Empcursor;
         End-proc Emploadsfl;

         Dcl-proc Empdspsfl;
           Dcl-pi Empdspsfl;
             W_Company Char(32);
             Pgmname   Char(10);
           End-pi;
           Sfldsp    = *On;
           Sflctl    = *On;
           Sflend    = *On;
           Dow Exit=*Off;
             If Emprrn<1;
               Sfldsp=*Off;
             Endif;
             Dhcomp   = W_Company;
             Dhheader = '       Manage Employee';
             Dprognam = Pgmname;
             Dfooter  = 'F3=Exit   F5=Refresh   F6=Create' ;
             Write Header;
             Write Footer;
             Exfmt Mngcemp;
             IF Create =*On;
               W_operation='C';
               Empcrudproc(W_Company:Pgmname:W_operation);
             Endif;
           Enddo;
           Reset Exit;
         End-proc Empdspsfl;

