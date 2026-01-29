     **free
         Ctl-opt Nomain;
         Dcl-f Mfg01dsp Workstn Indds(Indds) ;
         Dcl-ds Indds;
           Exit      Ind  Pos(03);
           Prompt    Ind  Pos(04);
           Refresh   Ind  Pos(05);
           Creupd    Ind  Pos(06);
           Cancel    Ind  Pos(12);
           Empgenrp  Ind  Pos(15);
           Empdobrp  Ind  Pos(16);
           Emprolrp  Ind  Pos(17);
           Empmobrp  Ind  Pos(18);
           Empaddrp  Ind  Pos(19);
           Empemarp  Ind  Pos(20);
           Empstarp  Ind  Pos(21);
           Empnamrp  Ind  Pos(22);
           Emppinrp  Ind  Pos(23);
           Empdojrp  Ind  Pos(24);
           Emppasrp  Ind  Pos(25);
           Empund    Ind  Pos(26);
           Passund   Ind  Pos(27);
           Nondsp    Ind  Pos(39);
         End-ds;
         Dcl-pr Stateproc;
           W_Company Char(32);
           Pgmname   Char(10);
           W_state   Char(20);
         End-pr;
         Dcl-s W_autoid Packed(6:0);
         Dcl-s W_state char(20);
         Dcl-s W_age Packed(2);
         Dcl-s W_pos1 Packed(2);
         DCl-s W_pos2 Packed(2);
         Dcl-s W_name Varchar(50);
         Dcl-s W_check Varchar(1) ;
         Dcl-s W_dobcheck Varchar(1);
         Dcl-s W_dojcheck Varchar(1);
         Dcl-s W_mobcheck Varchar(1);
         Dcl-s W_emacheck Varchar(1);
         Dcl-s W_empid1     Packed(6);
         dcl-s W_emailReg  char(48);
         Dcl-s W_emailResult Zoned(1);
         Dcl-s W_passcheck Varchar(1);
         Dcl-s W_pass1 Packed(2);
         Dcl-s W_addcheck Varchar(1);


         Dcl-proc Empcrudproc Export;
           Dcl-pi Empcrudproc;
             W_Company    Char  (32);
             Pgmname      Char  (10);
             W_operation  Char  (1);
             W_empid      Packed(6) options(*Nopass);
           End-pi;
           Dow Exit=*Off;
             Dhcomp   = W_Company;
             Dprognam = Pgmname;
             Select;
               When W_operation='C';
                 Dhheader = '     Create Employee';
                 Dfooter  = 'F3=Exit   F4=Prompt   F5=Refresh   F6=Create';
               When W_operation='U';
                 Dhheader = '       Update Employee';
                 Dfooter  = 'F3=Exit   F5=Refresh   F6=Update';
               When W_operation='D';
                 Dhheader = '     Display Employee';
                 Dfooter  = 'F3=Exit   ';
             Endsl;
             Write Header;
             Write Footer;
            Exec SQL SET Option Commit = *None;
             If %Parms()=4;
                If W_check=*Blanks;
                Exec Sql
                   Select EMpid,Empnam,emppass,empcon,empgen,
                   emptyp,empema,empage,empdob,empdoj,empstaT,
                   emppin,empadd
                    into :Dempid,:Dempnam,:Demppass,:Dempmob,
                 :Dempgen,:Demprol,:dempema,:dempage,
                 :Dempdob,:Dempdoj,:Dempsta,:Demppin,:Dempadd
                 from Mfgt02emp where Empid=:W_empid;
                 Endif;
               If W_operation='D';
                 Empund  = *Off;
                 Passund = *Off;
                 Nondsp  = *On;
                 Exfmt Empcrud;
                //    :Dempgen,:Demprol,:Demps
                Else;
                  Empund  = *On;
                  Passund = *On;
                  Nondsp  = *Off;
                  Exfmt Empcrud;
                  If Prompt=*On and Fld='DEMPGEN';
                 Prompt=*Off;
                 Dow Cancel=*Off;
                   Exfmt Empgenwin;
                   If Wchoice=*Blanks;
                      Werrmsg='Wchoice Cannot be Blank';
                    Elseif Wchoice='1';
                      Dempgen='M';
                      Leave;
                    Elseif Wchoice='2';
                      Dempgen='F';
                      Leave;
                    Elseif Wchoice='3';
                      Dempgen='O';
                      Leave;
                    Else;
                      Werrmsg='Invalid Choice ';
                    Endif;
                 Enddo;
                 Clear Werrmsg;
                 Clear Wchoice;
                 Cancel=*On;
               Endif;
               If Prompt=*On and Fld='DEMPSTA';
                 Prompt=*Off;
                 Stateproc(W_Company:Pgmname:W_state);
                 Dsply W_State;
                 Dempsta=W_state;
               Endif;
                If Prompt=*On and Fld='DEMPROL';
                  Prompt=*Off;
                  Dow Cancel=*Off;
                    Exfmt Emptypwin  ;
                    If Wchoice=*Blanks;
                      Werrmsg='Wchoice Cannot be Blank';
                    Elseif Wchoice='1';
                      Demprol='P';
                      Leave;
                    Elseif Wchoice='2';
                      Demprol='B';
                      Leave;
                    Elseif Wchoice='3';
                      Demprol='W';
                      Leave;
                    Elseif Wchoice='4';
                      Demprol='M';
                      Leave;
                    Else;
                      Werrmsg='Invalid Choice ';
                    Endif;
                  Enddo;
                  Clear Werrmsg;
                  Clear Wchoice;
                  Cancel=*Off;
               Endif;
                IF Exit=*On;
                 Exit=*Off;
                 Leave;
               Endif;
               IF Refresh=*On;
                 Exsr Turnoffrp;
                 Exsr ClearFields;
                 Clear W_check;
                 Clear W_dobcheck;
                 Clear W_dojcheck;
                 CLear W_mobcheck;
                 Clear W_emacheck;
                 Clear W_passcheck;
                 Clear W_addcheck;
                 Refresh=*Off;
               Else;
               Exsr Checkblankfields;
               If W_check     = 'Y';
                 Exsr Nameval;
                 Exsr Dobvalidate;
               Endif;
               If W_dobcheck  = 'Y';
                 Exsr Dojvalidate;
               Endif;
               If W_dojcheck  = 'Y';
                Exsr Mobilenumval;
               Endif;
               If W_mobcheck  = 'Y';
                 Exsr Emailval;
               Endif;
               If W_emacheck  = 'N';
                 Exsr Addval;
               Endif;
               IF W_addcheck='Y';
                 Exsr Passval;
               Endif;
               If W_passcheck='Y';
                 Exec Sql
                   Update Mfgt02emp
                   set Empid=:Dempid,
                       Empnam=:Dempnam,
                       Emppass=:Demppass,
                       Empcon =:Dempmob,
                       Empgen =:Dempgen,
                       Emptyp =:Demprol,
                       Empema =:Dempema,
                       Empage=:Dempage,
                       Empdob=:Dempdob,
                       Empdoj=:Dempdoj,
                       Empstat=:Dempsta,
                       Emppin=:Demppin,
                       Empadd=:Dempadd
                  where Empid=:W_empid;
                 Dsply Sqlcode;
                 Leave;
                 Endif;
               Endif;
               Endif;
             Else;
               Exsr Autoincempid;
               Empund  = *On;
               Passund = *On;
               Nondsp  = *Off;
               Dempid  = W_autoid;
               Exfmt EMPCRUD;
               If Prompt=*On and Fld='DEMPGEN';
                 Prompt=*Off;
                 Dow Cancel=*Off;
                   Exfmt Empgenwin;
                   If Wchoice=*Blanks;
                      Werrmsg='Wchoice Cannot be Blank';
                    Elseif Wchoice='1';
                      Dempgen='M';
                      Leave;
                    Elseif Wchoice='2';
                      Dempgen='F';
                      Leave;
                    Elseif Wchoice='3';
                      Dempgen='O';
                      Leave;
                    Else;
                      Werrmsg='Invalid Choice ';
                    Endif;
                 Enddo;
                 Clear Werrmsg;
                 Clear Wchoice;
                 Cancel=*On;
               Endif;
               If Prompt=*On and Fld='DEMPSTA';
                 Prompt=*Off;
                 Stateproc(W_Company:Pgmname:W_state);
                 Dsply W_State;
                 Dempsta=W_state;
               Endif;
               If Prompt=*On and Fld='DEMPROL';
                  Prompt=*Off;
                  Dow Cancel=*Off;
                    Exfmt Emptypwin  ;
                    If Wchoice=*Blanks;
                      Werrmsg='Wchoice Cannot be Blank';
                    Elseif Wchoice='1';
                      Demprol='P';
                      Leave;
                    Elseif Wchoice='2';
                      Demprol='B';
                      Leave;
                    Elseif Wchoice='3';
                      Demprol='W';
                      Leave;
                    Elseif Wchoice='4';
                      Demprol='M';
                      Leave;
                    Else;
                      Werrmsg='Invalid Choice ';
                    Endif;
                  Enddo;
                  Clear Werrmsg;
                  Clear Wchoice;
                  Cancel=*Off;
               Endif;
               IF Exit=*On;
                 Exit=*Off;
                 Leave;
               Endif;
               IF Refresh=*On;
                 Exsr Turnoffrp;
                 Exsr ClearFields;
                 Refresh=*Off;
               Else;
               Exsr Checkblankfields;
               If W_check     = 'Y';
                eXSR Nameval;
                 Exsr Dobvalidate;
               Endif;
               If W_dobcheck  = 'Y';
                 Exsr Dojvalidate;
               Endif;
               If W_dojcheck  = 'Y';
                Exsr Mobilenumval;
               Endif;
               If W_mobcheck  = 'Y';
                 Exsr Emailval;
               Endif;
               If W_emacheck  = 'N';
                 Exsr Passval;
               Endif;

               If W_passcheck='Y';
                 Exec Sql
                 Insert into Mfgt02emp
                 values(:Dempid,:Dempnam,:Demppass,:Dempmob,
                 :Dempgen,'',:Demprol,'A',:dempema,:dempage,
                 :Dempdob,:Dempdoj,:Dempsta,:Demppin,:Dempadd);
                 Dsply Sqlcode;
                 Leave;
                 Endif;
                          Endif;

             Endif;

           Enddo;
           Exsr ClearFields;
           Exsr Turnoffrp;
           Reset Exit;
           Clear W_check;
           Clear W_dobcheck;
           Clear W_dojcheck;
           CLear W_mobcheck;
           Clear W_emacheck;
           Clear W_passcheck;
           Clear W_addcheck;
           Begsr Autoincempid;
             Exec Sql
               Select Max(Empid) into :W_autoid from Mfgt02emp;
               W_autoid+=1;
           Endsr;
           Begsr Turnoffrp;
             Empnamrp = *Off;
             Empaddrp = *Off;
             Emppinrp = *Off;
             Empdobrp = *Off;
             Empemarp = *Off;
             Empmobrp = *Off;
             Empgenrp = *Off;
             Empstarp = *Off;
             Emppasrp = *Off;
             Emprolrp = *Off;
             Empdojrp = *Off;
             Emppinrp = *Off;
             Clear Derrmsg;
           Endsr;
           Begsr ClearFields;
             Clear Dempadd;
             Clear Dempage;
             Clear Dempdob;
             Clear Dempdoj;
             Clear Dempmob;
             Clear Dempnam;
             Clear Demppin;
             Clear Demppass;
             Clear Dempage;
             Clear Dempsta;
             Clear Demprol;
             Clear Derrmsg;
             Clear Dempgen;
             Clear Dempema;
           Endsr;

           Begsr Checkblankfields;
             If Dempnam=*Blanks;
                 Empnamrp = *On;
                 DERRMSG  = 'Employee Name cannot be Blank';
                 W_check='N';
               Elseif Dempgen=*Blanks;
                 Exsr Turnoffrp;
                 Empgenrp = *On;
                 Derrmsg  = 'Gender Field Cannot be Blank';
                 W_check='N';
               Elseif Dempdob=*Zeros;
                 Exsr Turnoffrp;
                 Empdobrp = *On;
                 DERRMSG  = 'Dob field Cannot be Blank';
                 W_check='N';
               Elseif Demppin=*Zeros;
                 Exsr Turnoffrp;
                 Emppinrp = *On;
                 DERRMSG  = 'Pincode field Cannot be Blank';
                 W_check='N';
               Elseif Demprol=*Blanks;
                 Exsr Turnoffrp;
                 Emprolrp = *On;
                 DERRMSG  = 'Role field Cannot be Blank';
                 W_check='N';
               Elseif Demprol=*Blanks;
                 Exsr Turnoffrp;
                 Emprolrp = *On;
                 DERRMSG  = 'Role field Cannot be Blank';
                W_check='N';
               Elseif Dempdoj=*Zeros;
                 Exsr Turnoffrp;
                 Empdojrp = *On;
                 DERRMSG  = 'Doj field Cannot be Blank';
                 W_check='N';
               Elseif Dempmob=*Zeros;
                 Exsr Turnoffrp;
                 Empmobrp = *On;
                 DERRMSG  = 'Mobile Number field Cannot be Blank';
                 W_check='N';
               Elseif Dempadd=*Blanks;
                 Exsr Turnoffrp;
                 Empaddrp = *On;
                 DERRMSG  = 'Address field Cannot be Blank';
                 W_check='N';
               Elseif Dempema=*Blanks;
                 Exsr Turnoffrp;
                 Empemarp = *On;
                 DERRMSG  = 'Email field Cannot be Blank';
                 W_check='N';
               Elseif Dempsta=*Blanks;
                 Exsr Turnoffrp;
                 Empstarp = *On;
                 DERRMSG  = 'State field Cannot be Blank';
                 W_check='N';
               Elseif Demppass=*Blanks;
                 Exsr Turnoffrp;
                 Emppasrp = *On;
                 DERRMSG  = 'Password field Cannot be Blank';
                 W_check='N';
               Elseif Demppin<100000;
                 Exsr Turnoffrp;
                 Empgenrp=*On;
                 Derrmsg='Pin Number is Not Valid';
                 W_check='N';
               Elseif Dempmob<6000000000;
                 Exsr Turnoffrp;
                 Empnamrp=*On;
                 Derrmsg='Mobile Number is Not Valid';
                 W_check='N';
               Else;
                 W_check='Y';
               Endif;
           Endsr;
           Begsr Dobvalidate;
              If Dempdob<>*Zeros;
                 Exsr Turnoffrp;
                 Test(DE) *Iso Dempdob;
                 If %Error();
                   Derrmsg='Invalid date of birth';
                   Empdobrp=*On;
                   W_dobcheck='N';
                 Else;
                   Exec Sql
                   Values  Year(CURRENT_DATE) - Year(:DEmpdob) into :Dempage;
                   If Dempage<18;
                     Dempage=*Zeros;
                     Empdobrp=*On;
                     Derrmsg='Employee Age should be greater than 18';
                     W_dobcheck='N';
                   Else;
                     W_dobcheck='Y';
                   Endif;
                 Endif;
              Endif;
           Endsr;
           Begsr Dojvalidate;
             If Dempdoj<>*Zeros;
               Exsr Turnoffrp;
               Test(DE) *Iso Dempdoj;
               If %Error();
                 Derrmsg='Invalid date of joining';
                 Empdojrp=*On;
                 W_dojcheck='N';
               Else;
                 Exec Sql
                   Values  Year(:Dempdoj) - Year(:DEmpdob) into :W_age;
                   If W_age<18;
                     Empdojrp=*On;
                     Derrmsg='Date of Join Cannot be ' +
                      'Greater than Date of Birth';
                     W_dojcheck='N';
                    Else;
                      W_dojcheck='Y';
                   Endif;
                 Endif;
                Endif;
           Endsr;
           Begsr Mobilenumval;
             Exsr Turnoffrp;
             Exec Sql

               Select Empid into :W_empid1 from MFGT02EMP
               where Empcon=:Dempmob;
             If W_empid1>100000 and W_empid1<>Dempid;
               Empmobrp=*On;
               W_mobcheck='N';
               Derrmsg='Mobile Number is Already present';
             Else;
               Clear W_empid1;
               W_mobcheck='Y';
             Endif;
           Endsr;

           Begsr Emailval;
            W_emailReg = '^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\.[a-zA-Z0-9-.]+' ;
            Exec sql
              values regexp_count(:Dempema , :W_emailReg) into :W_emailResult;
              If W_emailResult=1;
            Exec Sql
               Select Empid into :W_empid1 from MFGT02EMP
               where Empema=:Dempema;
            If W_empid1>100000;
               Clear W_empid1;
               W_emacheck='Y';
               Dsply 'hello';
             Else;
               Empemarp=*On;
               W_emacheck='N';
               Derrmsg='Email is Already present';
             Endif;
              Else;
                W_emacheck='N';
                Derrmsg='Email is  Not Valid';
              Endif;
           Endsr;
           Begsr Passval;

            //  If  %scan('@' : Demppass) > 0 or
            //      %scan('#' : Demppass) > 0 or
            //      %scan('$' : Demppass) > 0 or
            //      %scan('%' : Demppass) > 0 or
            //      %scan('^' : Demppass) > 0 or
            //      %scan('&' : Demppass) > 0 or
            //      %scan('*' : Demppass) > 0 or
            //      %scan('!' : Demppass) > 0;
                //  Dsply 'hii';
            //  Clear W_pass1;
                If  %scan('1' : Demppass) > 0 or
                 %scan('2' : Demppass) > 0 or
                 %scan('3' : Demppass) > 0 or
                 %scan('4' : Demppass) > 0 or
                 %scan('5' : Demppass) > 0 or
                 %scan('6' : Demppass) > 0 or
                 %scan('7' : Demppass) > 0 or
                 %scan('8' : Demppass) > 0 or
                 %scan('9' : Demppass) > 0  ;
                 Dsply 'hii';
            //    Clear W_pass1;
                  W_pass1=%len(%trim(Demppass));
               If W_pass1>=8;
                 clear W_pass1;
                 W_passcheck='Y';
               Else;
               W_passcheck='N';
               Emppasrp=*On;
               Derrmsg='Password must be greater than or Equal to 8 Character';
               Endif;
             Else;
              W_passcheck='N';
              Emppasrp=*On;
              Derrmsg='Password Requires Once Numeric Character';
             Endif;
          //  Else;
          //    W_passcheck='N';
          //    Emppasrp=*On;
          //    Derrmsg='Password Requires One Special Character';
          //  Endif;
           Endsr;

           Begsr Nameval;
               W_pos1=1;
               Dou W_pos1=0;             
                 Dempnam=%trim(Dempnam);   
                 W_pos1= %scan(' ':Dempnam);
                 W_name=W_name+' '+%subst(dEMPNAM:1:W_Pos1-1);
                 Dempnam=%subst(dEMPNAM:W_pos1+1); 
                 If %len(%trim(Dempnam))=0;    
                   leave;                   
                 endif;                    
              enddo; 
              Dempnam=%trim(W_name);   
              Clear W_name;                   
           Endsr;

           Begsr Addval;
            If %len(%trim(Dempadd))>15;
             W_addcheck='Y';
            Else;
              W_addcheck='N';
            Endif;
           Endsr;


         End-proc Empcrudproc;


