     *-------------------------------------------------------------        *
     * Program Name  : MFGADRPG2                                           *
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
          Dcl-f MFG01DSP Workstn Indds(Indds);

     ** ¬INDICATOR DATA STRUCTURE DECLARATION |
          Dcl-ds Indds;
            Refresh Ind Pos(05);
            Choice  Ind Pos(14);
          End-ds;

     ** ¬ PROCEDURE DECLARATION FOR MANAGE EMPLOYEE SCREEN |
          Dcl-pr Adempproc;
            W_Company Char   (32);
            Pgmname   Char   (10);
            W_emptyp  Varchar(1);
          End-pr;

     ** ¬ PROCEDURE DECLARATION FOR MANAGE LOCATION SCREEN |
          Dcl-pr Locproc;
            W_Company Char(32);
            Pgmname   Char(10);
          End-pr;

          Dcl-pr MngProproc;
              W_Company Char(32);
              Pgmname   Char(10);
              W_emptyp  Varchar(1);
              W_usrid   Packed(6);
          End-pr;

     ** ¬WORK VARIABLE DECLARATION |
          Dcl-s W_emptyp Varchar(1) Inz('A');
          Dcl-s Exit Zoned(1) Inz(1);

          Dcl-Proc Adhomeproc Export;
            Dcl-pi Adhomeproc;
              W_Company Char(32);
              Pgmname   Char(10);
              W_usrid   Packed(6);
            End-pi;

            Dow Exit=1;
              Dhcomp   = W_Company;
              Dhheader = '       Admin Options';
              Dprognam = Pgmname;
              Dfooter  = 'F5=Refresh';
              Dhopt1   = ' 1. Manage Employee Account';
              Dhopt2   = ' 2. Manage Location';
              Dhopt3   = ' 3. Manage Product';
              Dhopt4   = ' 4. Manage Bom';
              Dhopt5   = ' 5. Manage Work Order';
              Dhopt6   = ' 6. Manage Inventory';
              Dhopt7   = '99. Signoff';
              Write Header;
              Write Footer;
              Exfmt Home;

     ** ¬REFRESH FUNCTIONALITY |
              If Refresh = *On;
                Exsr Homeclear;
              Endif;

     **  ¬OPTION CHECK|
              If Dchoice <> *Zeros;

                Select;
                  When Dchoice = 1;
                    Exsr Homeclear;
                    Adempproc ( W_Company: Pgmname: W_emptyp);
                  When Dchoice = 99;
                    Exit = 0;
                  When Dchoice = 2;
                    Exsr Homeclear;
                    Locproc( W_Company: Pgmname);
                  When Dchoice = 3;
                    Exsr Homeclear;
                    MngProproc(W_Company: Pgmname: W_emptyp: W_usrid);
                Endsl;

              Else;
                Choice = *Off;
              Endif;
            Enddo;
           Exsr Homeclear;
      // Reset Exit;

     ** ¬SUBROUTINE FOR CLEAR FIELD|
           Begsr Homeclear;
             Clear Derrmsg;
             Clear Dchoice;
             Clear Dhopt1;
             Clear Dhopt2;
             Clear Dhopt3;
             Clear DHOPT4;
             Clear DHOPT5;
             Clear Dhopt6;
             Clear Dhopt7;
             Choice  = *off;
             Refresh = *Off;
             Exit    = 1;
           Endsr;

         End-Proc Adhomeproc;

