      *-------------------------------------------------------------       *
      * Program Name  : MFGRPG1                                             *
      * Description   : Manufacturing Inventory Management System           *
      *                                                                     *
      *                                                                     *
      * Developed By   : Archit Tambi                                       *
      * Role           : IBMi Intern                                        *
      * Language       : RPGLE (Free / Fixed \Z as applicable)              *
      * Created At     : 1/22/2026                                          *
      *                                                                     *
      *--------------------------------------------------------------------*
     HDFTACTGRP(*NO) Bnddir('MFGBND1')
      **free
          Dcl-f  Mfg01dsp Workstn Indds(Indds);
          Dcl-pr Adhomeproc ;
            W_Company Char(32);
            Pgmname   Char(10);
          End-pr;
          // Indicator data structure
          Dcl-ds  Indds;
            Exit    Ind  Pos(03);
            Prompt  Ind  Pos(04);
            Refresh Ind  Pos(05);
            Crup    Ind  Pos(06);
            Resete  Ind  Pos(11);
            Cancel  Ind  Pos(12);
            Usrrp   Ind  Pos(13);
          End-ds;
          // Work variable
          Dcl-s W_pass varchar(20);

          // Dataarea data structure for getting company name
          Dcl-ds DTDS DTAARA('MFGDTA01') ;
            W_Company CHAR(32);
          End-ds;
          Dcl-Ds PgmDs PSDS ;
            Pgmname Char(10) Pos(1);    // Extract Program Name
          End-Ds;
          In Dtds;
          Dow Exit=*Off;
            Dhheader = '        Sign In';
            Dhcomp   = W_Company;
            DFooter  = 'F3=Exit   F5=Refresh   F9=Forgot Password';
            Dprognam = Pgmname;
            Write Header;
            Write Footer;
            Exfmt Login;
            Select ;
              When Exit=*On;
                Leave;
              When Refresh=*On;
                Exsr Clrlogin;
              When Dusrid=*Zeros;
                Derrmsg = 'Userid Cannot be Blank';
                Usrrp   = *On;
              When Dusrid<>*Zeros;
                Usrrp=*off;
                Exec sql
                  Select EMPPASS into :W_pass from MFGT02EMP
                    where EMPID=:Dusrid;
                If W_pass=*Blanks and sqlcode=100;
                  Derrmsg='User does not Exist';
                Elseif DPASS=*Blanks ;
                    //   %Lower(DPASS)<> %Lower(W_pass);
                  Derrmsg='Invalid password. Please try again.';
                Else;
                  Exsr  Clrlogin;
                  Adhomeproc(W_Company:Pgmname);
                Endif;
            Endsl;
          Enddo;

          *Inlr=*On;

          Begsr Clrlogin;
            Clear Dusrid;
            Clear Dpass;
            Clear DERRMSG;
            Usrrp   = *Off;
            Refresh = *Off;
          Endsr;

