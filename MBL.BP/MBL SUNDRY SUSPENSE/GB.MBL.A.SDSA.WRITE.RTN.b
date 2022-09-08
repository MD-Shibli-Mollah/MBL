* @ValidationCode : MjotMjAxMjc0NzE0OkNwMTI1MjoxNjMwODE4NjE3ODA4OnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 05 Sep 2021 11:10:17
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0
SUBROUTINE GB.MBL.A.SDSA.WRITE.RTN
*-----------------------------------------------------------------------------
**Subroutine Description:
* THIS ROUTINE USE FOR WRITE RECORD IN BD.SDSA.ENTRY.DETAILS FILE
*Subroutine Type:
*Attached To    : Version (TELLER,MBL.SDSA.LCY.CASHIN TELLER,MBL.SDSA.LCY.CASHWDL TELLER,MBL.SDSA.LCY.CASHWDL.SUSP
*                         TELLER,MBL.SDSA.LCY.CASHIN.SUSP FUNDS.TRANSFER,MBL.SUSP.ORG FUNDS.TRANSFER,MBL.SUSP.ADJ FUNDS.TRANSFER,MBL.SUNDRY.ORG FUNDS.TRANSFER,MBL.SUNDRY.ADJ)
*Attached As    : AUTH ROUTINE
*-----------------------------------------------------------------------------
* Modification History :
* 28/06/2020 -                            Retrofit   - Sarowar Mortoza
*                                                  FDS Pvt Ltd

* 14/02/2021 - MODIFIED BY ---            MD SHIBLI MOLLAH --FDS Bangladesh Limited
*-----------------------------------------------------------------------------
  
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $USING ST.CompanyCreation
    $USING AC.AccountOpening
    $USING FT.Contract
    $USING TT.Contract
    $USING ST.Config
    $INSERT I_GTS.COMMON
    $USING EB.DataAccess
    $USING EB.Updates
    $USING EB.SystemTables
    $USING EB.TransactionControl
    $INSERT I_F.BD.SDSA.ENTRY.DETAILS
    $INSERT I_F.BD.SDSA.AC.REFNO

    IF EB.SystemTables.getVFunction() NE 'A' THEN RETURN
*-----------------------------------------------------------------------------
    GOSUB INITIALISE ; *
    GOSUB OPENFILE ; *
    GOSUB PROCESS ; *
RETURN
*-----------------------------------------------------------------------------

*** <region name= INITIALISE>
INITIALISE:
*** <desc> </desc>
    
    FN.AC = 'F.ACCOUNT'
    F.AC = ''

    FN.FT = 'F.FUNDS.TRANSFER'
    F.FT = ''

    FN.TT = 'F.TELLER'
    F.TT = ''

    FN.CAT = 'F.CATEGORY'
    F.CAT = ''

    FN.SDSA.DET = 'F.BD.SDSA.ENTRY.DETAILS'
    F.SDSA.DET = ''

    FN.SDSA.REF = 'F.BD.SDSA.AC.REFNO'
    F.SDSA.REF = ''
    
    Y.REF.NO = ''
    Y.SDSA.ORG.CNT = ''
    Y.SDSA.ADJ.CNT = ''
    Y.TOT.ADJ.AMT = 0
    Y.TOT.ORG.AMT = 0
    Y.TOT.OUT.AMT = 0
   
    Y.APP.NAME ="TELLER":@FM:"FUNDS.TRANSFER":@FM:"CATEGORY"
    LOCAL.FIELDS = ""
    LOCAL.FIELDS = "LT.ORG.ADJ":@VM:"LT.A.L":@VM:"LT.CIBT.SADN":@FM:"LT.ORG.ADJ":@VM:"LT.A.L":@FM:"LT.A.L"
    FLD.POS = ""
    EB.Updates.MultiGetLocRef(Y.APP.NAME, LOCAL.FIELDS,FLD.POS)
    Y.TT.ORGADJ.POS=FLD.POS<1,1>
    Y.TT.AL.POS=FLD.POS<1,2>
    Y.TT.SADN.POS=FLD.POS<1,3>
    Y.FT.ORGADJ.POS=FLD.POS<2,1>
    Y.FT.AL.POS=FLD.POS<2,2>
    Y.CAT.AL.POS=FLD.POS<3,1>
        
RETURN
*** </region>

*-----------------------------------------------------------------------------

*** <region name= OPENFILE>
OPENFILE:
*** <desc> </desc>
    EB.DataAccess.Opf(FN.AC,F.AC)
    EB.DataAccess.Opf(FN.FT,F.FT)
    EB.DataAccess.Opf(FN.TT,F.TT)
    EB.DataAccess.Opf(FN.CAT,F.CAT)
    EB.DataAccess.Opf(FN.SDSA.DET,F.SDSA.DET)
    EB.DataAccess.Opf(FN.SDSA.REF,F.SDSA.REF)
RETURN
*** </region>

*-----------------------------------------------------------------------------

*** <region name= PROCESS>
PROCESS:
    
*** <desc> </desc>
    IF EB.SystemTables.getApplication() EQ 'FUNDS.TRANSFER' THEN
        GOSUB FT.UPDATE.PROCESS ; *SUNDRY SUSPENSS FT TXN PROCESS
    END ELSE
        IF EB.SystemTables.getApplication() EQ 'TELLER' THEN
            GOSUB TT.UPDATE.PROCESS ; *SUNDRY SUSPENSS TT TXN PROCESS
        END
        RETURN
*** </region>

*-----------------------------------------------------------------------------

*** <region name= FT.UPDATE.PROCESS>
FT.UPDATE.PROCESS:
*** <desc>SUNDRY SUSPENSS FT TXN PROCESS </desc>
    
        BEGIN CASE
            CASE EB.SystemTables.getRNew(FT.Contract.FundsTransfer.LocalRef)<1,Y.FT.ORGADJ.POS> EQ 'ORG' AND EB.SystemTables.getRNew(FT.Contract.FundsTransfer.LocalRef)<1,Y.FT.AL.POS> EQ 'A'
                Y.REF.NO = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitTheirRef)
                EB.DataAccess.FRead(FN.SDSA.DET,Y.REF.NO,R.SDSA.DET.REC,F.SDSA.DET,Y.SDSA.DET.ERR)

                Y.DR.AC = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitAcctNo)
                EB.DataAccess.FRead(FN.AC,Y.DR.AC,AC.REC,F.AC,AC.ERR)
                Y.AC.CO.CODE = AC.REC<AC.AccountOpening.Account.CoCode>
   
                IF NOT(R.SDSA.DET.REC) THEN

                    Y.TODAY = EB.SystemTables.getToday()
                    Y.DR.AC.REF = Y.DR.AC :'-':Y.TODAY[3,2]
                    EB.DataAccess.FRead(FN.SDSA.REF,Y.DR.AC.REF,R.SDSA.REF.REC,F.SDSA.REF,Y.SDSA.REF.ERR)
                    Y.SDSA.REF.CNT = DCOUNT(R.SDSA.REF.REC,@FM) + 1
                    R.SDSA.REF.REC<BD.SDSA.AC.SDSA.REF.NO,Y.SDSA.REF.CNT> = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitTheirRef)
                    EB.DataAccess.FWrite(FN.SDSA.REF,Y.DR.AC.REF,R.SDSA.REF.REC)
                    EB.TransactionControl.JournalUpdate(Y.DR.AC.REF)
                
                    R.SDSA.DET.REC<BD.SDSA.AC.NUMBER> = Y.DR.AC
                    R.SDSA.DET.REC<BD.SDSA.AC.TYPE> = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.LocalRef)<1,Y.FT.AL.POS>
                    R.SDSA.DET.REC<BD.SDSA.ORG.TRANS.REF.NO,1> = EB.SystemTables.getIdNew()
                    R.SDSA.DET.REC<BD.SDSA.ORG.TRANS.CUR,1> = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitCurrency)
                    R.SDSA.DET.REC<BD.SDSA.ORG.PARTICULAR,1> = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.PaymentDetails)
                    R.SDSA.DET.REC<BD.SDSA.ORG.DATE,1> = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitValueDate)
                    R.SDSA.DET.REC<BD.SDSA.ORG.DRCR,1> = 'DR'
                    R.SDSA.DET.REC<BD.SDSA.ORG.AMT,1> = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitAmount)
                    R.SDSA.DET.REC<BD.SDSA.TOT.ORG.AMT> = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitAmount)
                    R.SDSA.DET.REC<BD.SDSA.OUTSTANDING.AMT> = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitAmount)
                    R.SDSA.DET.REC<BD.SDSA.CURR.NO> = '1'
                    R.SDSA.DET.REC<BD.SDSA.INPUTTER> = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.Inputter)
                    R.SDSA.DET.REC<BD.SDSA.DATE.TIME> = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DateTime)
                    R.SDSA.DET.REC<BD.SDSA.AUTHORISER> = EB.SystemTables.getOperator()
                    R.SDSA.DET.REC<BD.SDSA.CO.CODE> = Y.AC.CO.CODE
                    R.SDSA.DET.REC<BD.SDSA.DEPT.CODE> = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DeptCode)
                    EB.DataAccess.FWrite(FN.SDSA.DET,Y.REF.NO,R.SDSA.DET.REC)
                    EB.TransactionControl.JournalUpdate(Y.REF.NO)
                END ELSE
                    IF R.SDSA.DET.REC THEN

                        Y.SDSA.ORG.CNT = DCOUNT(R.SDSA.DET.REC<BD.SDSA.ORG.TRANS.REF.NO>,@VM) + 1
                        R.SDSA.DET.REC<BD.SDSA.ORG.TRANS.REF.NO,Y.SDSA.ORG.CNT> = EB.SystemTables.getIdNew()
                        R.SDSA.DET.REC<BD.SDSA.ORG.TRANS.CUR,Y.SDSA.ORG.CNT> = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitCurrency)
                        R.SDSA.DET.REC<BD.SDSA.ORG.PARTICULAR,Y.SDSA.ORG.CNT> = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.PaymentDetails)
                        R.SDSA.DET.REC<BD.SDSA.ORG.DATE,Y.SDSA.ORG.CNT> = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitValueDate)
                        R.SDSA.DET.REC<BD.SDSA.ORG.DRCR,Y.SDSA.ORG.CNT> = 'DR'
                        R.SDSA.DET.REC<BD.SDSA.ORG.AMT,Y.SDSA.ORG.CNT> = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitAmount)
                        Y.TOT.ORG.AMT = R.SDSA.DET.REC<BD.SDSA.TOT.ORG.AMT> + EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitAmount)
                        Y.TOT.OUT.AMT = R.SDSA.DET.REC<BD.SDSA.OUTSTANDING.AMT> + EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitAmount)
                        R.SDSA.DET.REC<BD.SDSA.TOT.ORG.AMT> = Y.TOT.ORG.AMT
                        R.SDSA.DET.REC<BD.SDSA.OUTSTANDING.AMT> = Y.TOT.OUT.AMT
                        R.SDSA.DET.REC<BD.SDSA.CURR.NO> += 1
                        R.SDSA.DET.REC<BD.SDSA.INPUTTER> = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.Inputter)
                        R.SDSA.DET.REC<BD.SDSA.DATE.TIME> = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DateTime)
                        R.SDSA.DET.REC<BD.SDSA.AUTHORISER> = EB.SystemTables.getOperator()
                        R.SDSA.DET.REC<BD.SDSA.CO.CODE> = Y.AC.CO.CODE
                        R.SDSA.DET.REC<BD.SDSA.DEPT.CODE> = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DeptCode)
                        EB.DataAccess.FWrite(FN.SDSA.DET,Y.REF.NO,R.SDSA.DET.REC)
                        EB.TransactionControl.JournalUpdate(Y.REF.NO)
                    END
                END

            CASE EB.SystemTables.getRNew(FT.Contract.FundsTransfer.LocalRef)<1,Y.FT.ORGADJ.POS> EQ 'ADJ' AND EB.SystemTables.getRNew(FT.Contract.FundsTransfer.LocalRef)<1,Y.FT.AL.POS> EQ 'A'
                IF EB.SystemTables.getRNew(FT.Contract.FundsTransfer.CreditTheirRef) NE '' THEN
                    Y.REF.NO = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.CreditTheirRef)
                    EB.DataAccess.FRead(FN.SDSA.DET,Y.REF.NO,R.SDSA.DET.REC,F.SDSA.DET,Y.SDSA.DET.ERR)
                    IF R.SDSA.DET.REC THEN
                        Y.SDSA.ADJ.CNT = DCOUNT(R.SDSA.DET.REC<BD.SDSA.ADJ.TRANS.REF.NO>,@VM) + 1
                        Y.TOT.ADJ.AMT = R.SDSA.DET.REC<BD.SDSA.TOT.ADJ.AMT> + EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitAmount)
                        Y.TOT.OUT.AMT = R.SDSA.DET.REC<BD.SDSA.OUTSTANDING.AMT> - EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitAmount)
                        R.SDSA.DET.REC<BD.SDSA.ADJ.TRANS.REF.NO,Y.SDSA.ADJ.CNT> = EB.SystemTables.getIdNew()
                        R.SDSA.DET.REC<BD.SDSA.ADJ.TRANS.CUR,Y.SDSA.ADJ.CNT> = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitCurrency)
                        R.SDSA.DET.REC<BD.SDSA.ADJ.PARTICULAR,Y.SDSA.ADJ.CNT> = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.PaymentDetails)
                        R.SDSA.DET.REC<BD.SDSA.ADJ.DATE,Y.SDSA.ADJ.CNT> = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitValueDate)
                        R.SDSA.DET.REC<BD.SDSA.ADJ.DRCR,Y.SDSA.ADJ.CNT> = 'CR'
                        R.SDSA.DET.REC<BD.SDSA.ADJ.AMT,Y.SDSA.ADJ.CNT> = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitAmount)
                        R.SDSA.DET.REC<BD.SDSA.TOT.ADJ.AMT> = Y.TOT.ADJ.AMT
                        R.SDSA.DET.REC<BD.SDSA.OUTSTANDING.AMT> = Y.TOT.OUT.AMT
                        R.SDSA.DET.REC<BD.SDSA.CURR.NO> += 1
                        R.SDSA.DET.REC<BD.SDSA.INPUTTER> = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.Inputter)
                        R.SDSA.DET.REC<BD.SDSA.DATE.TIME> = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DateTime)
                        R.SDSA.DET.REC<BD.SDSA.AUTHORISER> = EB.SystemTables.getOperator()
                        R.SDSA.DET.REC<BD.SDSA.DEPT.CODE> = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DeptCode)
                        EB.DataAccess.FWrite(FN.SDSA.DET,Y.REF.NO,R.SDSA.DET.REC)
                        EB.TransactionControl.JournalUpdate(Y.REF.NO)
                    END
                END
            CASE EB.SystemTables.getRNew(FT.Contract.FundsTransfer.LocalRef)<1,Y.FT.ORGADJ.POS> EQ 'ORG' AND EB.SystemTables.getRNew(FT.Contract.FundsTransfer.LocalRef)<1,Y.FT.AL.POS> EQ 'L'
                Y.REF.NO = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.CreditTheirRef)
                EB.DataAccess.FRead(FN.SDSA.DET,Y.REF.NO,R.SDSA.DET.REC,F.SDSA.DET,Y.SDSA.DET.ERR)

                Y.CR.AC = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.CreditAcctNo)
                EB.DataAccess.FRead(FN.AC,Y.CR.AC,AC.REC,F.AC,AC.ERR)
                Y.AC.CO.CODE = AC.REC<AC.AccountOpening.Account.CoCode>

                IF NOT(R.SDSA.DET.REC) THEN
                    Y.TODAY = EB.SystemTables.getToday()
                    Y.CR.AC.REF = Y.CR.AC :'-':Y.TODAY[3,2]
                    EB.DataAccess.FRead(FN.SDSA.REF,Y.CR.AC.REF,R.SDSA.REF.REC,F.SDSA.REF,Y.SDSA.REF.ERR)
                    Y.SDSA.REF.CNT = DCOUNT(R.SDSA.REF.REC,@FM) + 1
                    R.SDSA.REF.REC<BD.SDSA.AC.SDSA.REF.NO,Y.SDSA.REF.CNT> = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.CreditTheirRef)
                    EB.DataAccess.FWrite(FN.SDSA.REF,Y.CR.AC.REF,R.SDSA.REF.REC)
                    EB.TransactionControl.JournalUpdate(Y.CR.AC.REF)

                    R.SDSA.DET.REC<BD.SDSA.AC.NUMBER> = Y.CR.AC
                    R.SDSA.DET.REC<BD.SDSA.AC.TYPE> = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.LocalRef)<1,Y.FT.AL.POS>
                    R.SDSA.DET.REC<BD.SDSA.ORG.TRANS.REF.NO,1> = EB.SystemTables.getIdNew()
                    R.SDSA.DET.REC<BD.SDSA.ORG.TRANS.CUR,1> = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitCurrency)
                    R.SDSA.DET.REC<BD.SDSA.ORG.PARTICULAR,1> = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.PaymentDetails)
                    R.SDSA.DET.REC<BD.SDSA.ORG.DATE,1> = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitValueDate)
                    R.SDSA.DET.REC<BD.SDSA.ORG.DRCR,1> = 'CR'
                    R.SDSA.DET.REC<BD.SDSA.ORG.AMT,1> = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitAmount)
                    R.SDSA.DET.REC<BD.SDSA.TOT.ORG.AMT> = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitAmount)
                    R.SDSA.DET.REC<BD.SDSA.OUTSTANDING.AMT> = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitAmount)
                    R.SDSA.DET.REC<BD.SDSA.CURR.NO> = '1'
                    R.SDSA.DET.REC<BD.SDSA.INPUTTER> = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.Inputter)
                    R.SDSA.DET.REC<BD.SDSA.DATE.TIME> = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DateTime)
                    R.SDSA.DET.REC<BD.SDSA.AUTHORISER> = EB.SystemTables.getOperator()
                    R.SDSA.DET.REC<BD.SDSA.CO.CODE> = Y.AC.CO.CODE
                    R.SDSA.DET.REC<BD.SDSA.DEPT.CODE> = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DeptCode)
                    EB.DataAccess.FWrite(FN.SDSA.DET,Y.REF.NO,R.SDSA.DET.REC)
                    EB.TransactionControl.JournalUpdate(Y.REF.NO)
                END ELSE
                    IF R.SDSA.DET.REC THEN

                        Y.SDSA.ORG.CNT = DCOUNT(R.SDSA.DET.REC<BD.SDSA.ORG.TRANS.REF.NO>,@VM) + 1
                        R.SDSA.DET.REC<BD.SDSA.ORG.TRANS.REF.NO,Y.SDSA.ORG.CNT> = EB.SystemTables.getIdNew()
                        R.SDSA.DET.REC<BD.SDSA.ORG.TRANS.CUR,Y.SDSA.ORG.CNT> = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitCurrency)
                        R.SDSA.DET.REC<BD.SDSA.ORG.PARTICULAR,Y.SDSA.ORG.CNT> = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.PaymentDetails)
                        R.SDSA.DET.REC<BD.SDSA.ORG.DATE,Y.SDSA.ORG.CNT> = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitValueDate)
                        R.SDSA.DET.REC<BD.SDSA.ORG.DRCR,Y.SDSA.ORG.CNT> = 'CR'
                        R.SDSA.DET.REC<BD.SDSA.ORG.AMT,Y.SDSA.ORG.CNT> = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitAmount)
                        Y.TOT.ORG.AMT = R.SDSA.DET.REC<BD.SDSA.TOT.ORG.AMT> + EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitAmount)
                        Y.TOT.OUT.AMT = R.SDSA.DET.REC<BD.SDSA.OUTSTANDING.AMT> + EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitAmount)
                        R.SDSA.DET.REC<BD.SDSA.TOT.ORG.AMT> = Y.TOT.ORG.AMT
                        R.SDSA.DET.REC<BD.SDSA.OUTSTANDING.AMT> = Y.TOT.OUT.AMT
                        R.SDSA.DET.REC<BD.SDSA.CURR.NO> += 1
                        R.SDSA.DET.REC<BD.SDSA.INPUTTER> = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.Inputter)
                        R.SDSA.DET.REC<BD.SDSA.DATE.TIME> = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DateTime)
                        R.SDSA.DET.REC<BD.SDSA.AUTHORISER> = EB.SystemTables.getOperator()
                        R.SDSA.DET.REC<BD.SDSA.CO.CODE> = Y.AC.CO.CODE
                        R.SDSA.DET.REC<BD.SDSA.DEPT.CODE> = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DeptCode)
                        EB.DataAccess.FWrite(FN.SDSA.DET,Y.REF.NO,R.SDSA.DET.REC)
                        EB.TransactionControl.JournalUpdate(Y.REF.NO)
                    END
                END
            CASE EB.SystemTables.getRNew(FT.Contract.FundsTransfer.LocalRef)<1,Y.FT.ORGADJ.POS> EQ 'ADJ' AND EB.SystemTables.getRNew(FT.Contract.FundsTransfer.LocalRef)<1,Y.FT.AL.POS> EQ 'L'
                IF EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitTheirRef) NE '' THEN
                    Y.REF.NO = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitTheirRef)
                    EB.DataAccess.FRead(FN.SDSA.DET,Y.REF.NO,R.SDSA.DET.REC,F.SDSA.DET,Y.SDSA.DET.ERR)
                    IF R.SDSA.DET.REC THEN
                        Y.SDSA.ADJ.CNT = DCOUNT(R.SDSA.DET.REC<BD.SDSA.ADJ.DATE>,@VM) + 1
                        Y.TOT.ADJ.AMT = R.SDSA.DET.REC<BD.SDSA.TOT.ADJ.AMT> + EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitAmount)
                        Y.TOT.OUT.AMT = R.SDSA.DET.REC<BD.SDSA.OUTSTANDING.AMT> - EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitAmount)
                        R.SDSA.DET.REC<BD.SDSA.ADJ.TRANS.REF.NO,Y.SDSA.ADJ.CNT> = EB.SystemTables.getIdNew()
                        R.SDSA.DET.REC<BD.SDSA.ADJ.TRANS.CUR,Y.SDSA.ADJ.CNT> = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitCurrency)
                        R.SDSA.DET.REC<BD.SDSA.ADJ.PARTICULAR,Y.SDSA.ADJ.CNT> = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.PaymentDetails)
                        R.SDSA.DET.REC<BD.SDSA.ADJ.DATE,Y.SDSA.ADJ.CNT> = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitValueDate)
                        R.SDSA.DET.REC<BD.SDSA.ADJ.DRCR,Y.SDSA.ADJ.CNT> = 'DR'
                        R.SDSA.DET.REC<BD.SDSA.ADJ.AMT,Y.SDSA.ADJ.CNT> = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitAmount)
                        R.SDSA.DET.REC<BD.SDSA.TOT.ADJ.AMT> = Y.TOT.ADJ.AMT
                        R.SDSA.DET.REC<BD.SDSA.OUTSTANDING.AMT> = Y.TOT.OUT.AMT
                        R.SDSA.DET.REC<BD.SDSA.CURR.NO> += 1
                        R.SDSA.DET.REC<BD.SDSA.INPUTTER> = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.Inputter)
                        R.SDSA.DET.REC<BD.SDSA.DATE.TIME> = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DateTime)
                        R.SDSA.DET.REC<BD.SDSA.AUTHORISER> = EB.SystemTables.getOperator()
                        R.SDSA.DET.REC<BD.SDSA.DEPT.CODE> = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DeptCode)
                        EB.DataAccess.FWrite(FN.SDSA.DET,Y.REF.NO,R.SDSA.DET.REC)
                        EB.TransactionControl.JournalUpdate(Y.REF.NO)
                    END
                END
        END CASE
        RETURN
*** </region>


*-----------------------------------------------------------------------------

*** <region name= TT.UPDATE.PROCESS>
TT.UPDATE.PROCESS:
*** <desc>SUNDRY SUSPENSS TT TXN PROCESS </desc>
*        WRITE.FILE.VAR = 'ENTERING TT PROCESS'
*        GOSUB FILE.WRITE
        BEGIN CASE
            CASE EB.SystemTables.getRNew(TT.Contract.Teller.TeLocalRef)<1,Y.TT.ORGADJ.POS> EQ 'ORG' AND EB.SystemTables.getRNew(TT.Contract.Teller.TeLocalRef)<1,Y.TT.AL.POS> EQ 'A'
                Y.REF.NO = EB.SystemTables.getRNew(TT.Contract.Teller.TeOurReference)
                EB.DataAccess.FRead(FN.SDSA.DET,Y.REF.NO,R.SDSA.DET.REC,F.SDSA.DET,Y.SDSA.DET.ERR)
*                WRITE.FILE.VAR = 'LINE 307 R.SDSA.DET.REC: ':R.SDSA.DET.REC:' Y.REF.NO: ':Y.REF.NO
*                GOSUB FILE.WRITE
            
                IF NOT(R.SDSA.DET.REC) THEN

                    IF EB.SystemTables.getRNew(TT.Contract.Teller.TeDrCrMarker) EQ 'CREDIT' THEN
                        Y.DR.AC = EB.SystemTables.getRNew(TT.Contract.Teller.TeAccountTwo)
                    END ELSE
                        Y.DR.AC = EB.SystemTables.getRNew(TT.Contract.Teller.TeAccountOne)
                    END
            
*                    WRITE.FILE.VAR = 'LINE 318 Y.DR.AC: ':Y.DR.AC
*                    GOSUB FILE.WRITE
                
                    EB.DataAccess.FRead(FN.AC,Y.DR.AC,AC.REC,F.AC,AC.ERR)
                    Y.AC.CO.CODE = AC.REC<AC.AccountOpening.Account.CoCode>

                    Y.TODAY = EB.SystemTables.getToday()
                    Y.DR.AC.REF = Y.DR.AC :'-':Y.TODAY[3,2]
                
*                    WRITE.FILE.VAR = 'LINE 327 Y.DR.AC: ':Y.DR.AC:' Y.AC.CO.CODE: ':Y.AC.CO.CODE:' Y.DR.AC.REF: ':Y.DR.AC.REF
*                    GOSUB FILE.WRITE
                
                    EB.DataAccess.FRead(FN.SDSA.REF,Y.DR.AC.REF,R.SDSA.REF.REC,F.SDSA.REF,Y.SDSA.REF.ERR)
                    Y.SDSA.REF.CNT = DCOUNT(R.SDSA.REF.REC,@FM) + 1
                    R.SDSA.REF.REC<BD.SDSA.AC.SDSA.REF.NO,Y.SDSA.REF.CNT> = EB.SystemTables.getRNew(TT.Contract.Teller.TeOurReference)
                    EB.DataAccess.FWrite(FN.SDSA.REF,Y.DR.AC.REF,R.SDSA.REF.REC)
                    EB.TransactionControl.JournalUpdate(Y.DR.AC.REF)
                
*                    WRITE.FILE.VAR = 'LINE 336 R.SDSA.REF.REC: ':R.SDSA.REF.REC:' Y.SDSA.REF.CNT: ':Y.SDSA.REF.CNT:' R.SDSA.REF.REC: ':R.SDSA.REF.REC
*                    GOSUB FILE.WRITE

                    R.SDSA.DET.REC<BD.SDSA.AC.NUMBER> = Y.DR.AC
                    R.SDSA.DET.REC<BD.SDSA.AC.TYPE> = EB.SystemTables.getRNew(TT.Contract.Teller.TeLocalRef)<1,Y.TT.AL.POS>
                    R.SDSA.DET.REC<BD.SDSA.ORG.TRANS.REF.NO,1> = EB.SystemTables.getIdNew()
                    R.SDSA.DET.REC<BD.SDSA.ORG.TRANS.CUR,1> = EB.SystemTables.getRNew(TT.Contract.Teller.TeCurrencyOne)
                    R.SDSA.DET.REC<BD.SDSA.ORG.PARTICULAR,1> = EB.SystemTables.getRNew(TT.Contract.Teller.TeNarrativeTwo)
                    R.SDSA.DET.REC<BD.SDSA.ORG.DATE,1> = EB.SystemTables.getRNew(TT.Contract.Teller.TeValueDateTwo)
                    R.SDSA.DET.REC<BD.SDSA.ORG.DRCR,1> = 'DR'
                    R.SDSA.DET.REC<BD.SDSA.ORG.AMT,1> = EB.SystemTables.getRNew(TT.Contract.Teller.TeAmountLocalOne)
                    R.SDSA.DET.REC<BD.SDSA.TOT.ORG.AMT> = EB.SystemTables.getRNew(TT.Contract.Teller.TeAmountLocalOne)
                    R.SDSA.DET.REC<BD.SDSA.OUTSTANDING.AMT> = EB.SystemTables.getRNew(TT.Contract.Teller.TeAmountLocalOne)
                    R.SDSA.DET.REC<BD.SDSA.CURR.NO> = '1'
                    R.SDSA.DET.REC<BD.SDSA.INPUTTER> = EB.SystemTables.getRNew(TT.Contract.Teller.TeInputter)
                    R.SDSA.DET.REC<BD.SDSA.DATE.TIME> = EB.SystemTables.getRNew(TT.Contract.Teller.TeDateTime)
                    R.SDSA.DET.REC<BD.SDSA.AUTHORISER> = EB.SystemTables.getOperator()
                    R.SDSA.DET.REC<BD.SDSA.CO.CODE> = Y.AC.CO.CODE
                    R.SDSA.DET.REC<BD.SDSA.DEPT.CODE> = EB.SystemTables.getRNew(TT.Contract.Teller.TeDeptCode)
                    EB.DataAccess.FWrite(FN.SDSA.DET,Y.REF.NO,R.SDSA.DET.REC)
                    EB.TransactionControl.JournalUpdate(Y.REF.NO)
                
*                    WRITE.FILE.VAR = 'FROM 358 Y.DR.AC: ':Y.DR.AC:' Y.DR.AC.REF: ':Y.DR.AC.REF:' R.SDSA.REF.REC: ':R.SDSA.REF.REC:' R.SDSA.DET.REC: ':R.SDSA.DET.REC:' Y.AC.CO.CODE: ':Y.AC.CO.CODE
*                    GOSUB FILE.WRITE
                END ELSE
                    IF R.SDSA.DET.REC THEN

                        Y.SDSA.ORG.CNT = DCOUNT(R.SDSA.DET.REC<BD.SDSA.ORG.TRANS.REF.NO>,@VM) + 1
                        R.SDSA.DET.REC<BD.SDSA.ORG.TRANS.REF.NO,Y.SDSA.ORG.CNT> = EB.SystemTables.getIdNew()
                        R.SDSA.DET.REC<BD.SDSA.ORG.TRANS.CUR,Y.SDSA.ORG.CNT> = EB.SystemTables.getRNew(TT.Contract.Teller.TeCurrencyOne)
                        R.SDSA.DET.REC<BD.SDSA.ORG.PARTICULAR,Y.SDSA.ORG.CNT> = EB.SystemTables.getRNew(TT.Contract.Teller.TeNarrativeTwo)
                        R.SDSA.DET.REC<BD.SDSA.ORG.DATE,Y.SDSA.ORG.CNT> = EB.SystemTables.getRNew(TT.Contract.Teller.TeValueDateTwo)
                        R.SDSA.DET.REC<BD.SDSA.ORG.DRCR,Y.SDSA.ORG.CNT> = 'DR'
                        R.SDSA.DET.REC<BD.SDSA.ORG.AMT,Y.SDSA.ORG.CNT> = EB.SystemTables.getRNew(TT.Contract.Teller.TeAmountLocalOne)
                        Y.TOT.ORG.AMT = R.SDSA.DET.REC<BD.SDSA.TOT.ORG.AMT> + EB.SystemTables.getRNew(TT.Contract.Teller.TeAmountLocalOne)
                        Y.TOT.OUT.AMT = R.SDSA.DET.REC<BD.SDSA.OUTSTANDING.AMT> + EB.SystemTables.getRNew(TT.Contract.Teller.TeAmountLocalOne)
                        R.SDSA.DET.REC<BD.SDSA.TOT.ORG.AMT> = Y.TOT.ORG.AMT
                        R.SDSA.DET.REC<BD.SDSA.OUTSTANDING.AMT> = Y.TOT.OUT.AMT
                        R.SDSA.DET.REC<BD.SDSA.CURR.NO> += 1
                        R.SDSA.DET.REC<BD.SDSA.INPUTTER> = EB.SystemTables.getRNew(TT.Contract.Teller.TeInputter)
                        R.SDSA.DET.REC<BD.SDSA.DATE.TIME> = EB.SystemTables.getRNew(TT.Contract.Teller.TeDateTime)
                        R.SDSA.DET.REC<BD.SDSA.AUTHORISER> = EB.SystemTables.getOperator()
                        R.SDSA.DET.REC<BD.SDSA.CO.CODE> = Y.AC.CO.CODE
                        R.SDSA.DET.REC<BD.SDSA.DEPT.CODE> = EB.SystemTables.getRNew(TT.Contract.Teller.TeDeptCode)
                        EB.DataAccess.FWrite(FN.SDSA.DET,Y.REF.NO,R.SDSA.DET.REC)
                        EB.TransactionControl.JournalUpdate(Y.REF.NO)
                    END
*                    WRITE.FILE.VAR = 'FROM 383 Y.DR.AC: ':Y.DR.AC:' Y.DR.AC.REF: ':Y.DR.AC.REF:' R.SDSA.REF.REC: ':R.SDSA.REF.REC:' R.SDSA.DET.REC: ':R.SDSA.DET.REC:' Y.AC.CO.CODE: ':Y.AC.CO.CODE
*                    GOSUB FILE.WRITE
                END

            CASE EB.SystemTables.getRNew(TT.Contract.Teller.TeLocalRef)<1,Y.TT.ORGADJ.POS> EQ 'ADJ' AND EB.SystemTables.getRNew(TT.Contract.Teller.TeLocalRef)<1,Y.TT.AL.POS> EQ 'A'

                IF EB.SystemTables.getRNew(TT.Contract.Teller.TeLocalRef)<1,Y.TT.SADN.POS> NE '' THEN
                    Y.REF.NO = EB.SystemTables.getRNew(TT.Contract.Teller.TeLocalRef)<1,Y.TT.SADN.POS>

                    EB.DataAccess.FRead(FN.SDSA.DET,Y.REF.NO,R.SDSA.DET.REC,F.SDSA.DET,Y.SDSA.DET.ERR)
                    IF R.SDSA.DET.REC THEN
                        Y.SDSA.ADJ.CNT = DCOUNT(R.SDSA.DET.REC<BD.SDSA.ADJ.DATE>,@VM) + 1
                        Y.TOT.ADJ.AMT = R.SDSA.DET.REC<BD.SDSA.TOT.ADJ.AMT> + EB.SystemTables.getRNew(TT.Contract.Teller.TeAmountLocalOne)
                        Y.TOT.OUT.AMT = R.SDSA.DET.REC<BD.SDSA.OUTSTANDING.AMT> - EB.SystemTables.getRNew(TT.Contract.Teller.TeAmountLocalOne)
                        R.SDSA.DET.REC<BD.SDSA.ADJ.TRANS.REF.NO,Y.SDSA.ADJ.CNT> = EB.SystemTables.getIdNew()
                        R.SDSA.DET.REC<BD.SDSA.ADJ.TRANS.CUR,Y.SDSA.ADJ.CNT> = EB.SystemTables.getRNew(TT.Contract.Teller.TeCurrencyOne)
                        R.SDSA.DET.REC<BD.SDSA.ADJ.PARTICULAR,Y.SDSA.ADJ.CNT> = EB.SystemTables.getRNew(TT.Contract.Teller.TeNarrativeTwo)
                        R.SDSA.DET.REC<BD.SDSA.ADJ.DATE,Y.SDSA.ADJ.CNT> = EB.SystemTables.getRNew(TT.Contract.Teller.TeValueDateTwo)
                        R.SDSA.DET.REC<BD.SDSA.ADJ.DRCR,Y.SDSA.ADJ.CNT> = 'CR'
                        R.SDSA.DET.REC<BD.SDSA.ADJ.AMT,Y.SDSA.ADJ.CNT> = EB.SystemTables.getRNew(TT.Contract.Teller.TeAmountLocalOne)
                        R.SDSA.DET.REC<BD.SDSA.TOT.ADJ.AMT> = Y.TOT.ADJ.AMT
                        R.SDSA.DET.REC<BD.SDSA.OUTSTANDING.AMT> = Y.TOT.OUT.AMT
                        R.SDSA.DET.REC<BD.SDSA.CURR.NO> += 1
                        R.SDSA.DET.REC<BD.SDSA.INPUTTER> = EB.SystemTables.getRNew(TT.Contract.Teller.TeInputter)
                        R.SDSA.DET.REC<BD.SDSA.DATE.TIME> = EB.SystemTables.getRNew(TT.Contract.Teller.TeDateTime)
                        R.SDSA.DET.REC<BD.SDSA.AUTHORISER> = EB.SystemTables.getOperator()
                        R.SDSA.DET.REC<BD.SDSA.DEPT.CODE> = EB.SystemTables.getRNew(TT.Contract.Teller.TeDeptCode)
                        EB.DataAccess.FWrite(FN.SDSA.DET,Y.REF.NO,R.SDSA.DET.REC)
                        EB.TransactionControl.JournalUpdate(Y.REF.NO)
                    END
*                    WRITE.FILE.VAR = 'FROM 413 Y.DR.AC: ':Y.DR.AC:' Y.DR.AC.REF: ':Y.DR.AC.REF:' R.SDSA.REF.REC: ':R.SDSA.REF.REC:' R.SDSA.DET.REC: ':R.SDSA.DET.REC:' Y.AC.CO.CODE: ':Y.AC.CO.CODE
*                    GOSUB FILE.WRITE
                END
            CASE EB.SystemTables.getRNew(TT.Contract.Teller.TeLocalRef)<1,Y.TT.ORGADJ.POS> EQ 'ORG' AND EB.SystemTables.getRNew(TT.Contract.Teller.TeLocalRef)<1,Y.TT.AL.POS> EQ 'L'
                Y.REF.NO = EB.SystemTables.getRNew(TT.Contract.Teller.TeOurReference)
                EB.DataAccess.FRead(FN.SDSA.DET,Y.REF.NO,R.SDSA.DET.REC,F.SDSA.DET,Y.SDSA.DET.ERR)
                IF NOT(R.SDSA.DET.REC) THEN
                    Y.CR.AC = EB.SystemTables.getRNew(TT.Contract.Teller.TeAccountTwo)

                    EB.DataAccess.FRead(FN.AC,Y.CR.AC,AC.REC,F.AC,AC.ERR)
                    Y.AC.CO.CODE = AC.REC<AC.AccountOpening.Account.CoCode>
                    
                    Y.TODAY = EB.SystemTables.getToday()
                    Y.CR.AC.REF = Y.CR.AC :'-':Y.TODAY[3,2]
                    EB.DataAccess.FRead(FN.SDSA.REF,Y.CR.AC.REF,R.SDSA.REF.REC,F.SDSA.REF,Y.SDSA.REF.ERR)
                    Y.SDSA.REF.CNT = DCOUNT(R.SDSA.REF.REC,@FM) + 1
                    R.SDSA.REF.REC<BD.SDSA.AC.SDSA.REF.NO,Y.SDSA.REF.CNT> = EB.SystemTables.getRNew(TT.Contract.Teller.TeOurReference)
                    EB.DataAccess.FWrite(FN.SDSA.REF,Y.CR.AC.REF,R.SDSA.REF.REC)
                    EB.TransactionControl.JournalUpdate(Y.CR.AC.REF)

                    R.SDSA.DET.REC<BD.SDSA.AC.NUMBER> = Y.CR.AC
                    R.SDSA.DET.REC<BD.SDSA.AC.TYPE> = EB.SystemTables.getRNew(TT.Contract.Teller.TeLocalRef)<1,Y.TT.AL.POS>
                    R.SDSA.DET.REC<BD.SDSA.ORG.TRANS.REF.NO,1> = EB.SystemTables.getIdNew()
                    R.SDSA.DET.REC<BD.SDSA.ORG.TRANS.CUR,1> = EB.SystemTables.getRNew(TT.Contract.Teller.TeCurrencyOne)
                    R.SDSA.DET.REC<BD.SDSA.ORG.PARTICULAR,1> = EB.SystemTables.getRNew(TT.Contract.Teller.TeNarrativeTwo)
                    R.SDSA.DET.REC<BD.SDSA.ORG.DATE,1> = EB.SystemTables.getRNew(TT.Contract.Teller.TeValueDateTwo)
                    R.SDSA.DET.REC<BD.SDSA.ORG.DRCR,1> = 'CR'
                    R.SDSA.DET.REC<BD.SDSA.ORG.AMT,1> = EB.SystemTables.getRNew(TT.Contract.Teller.TeAmountLocalOne)
                    R.SDSA.DET.REC<BD.SDSA.TOT.ORG.AMT> = EB.SystemTables.getRNew(TT.Contract.Teller.TeAmountLocalOne)
                    R.SDSA.DET.REC<BD.SDSA.OUTSTANDING.AMT> = EB.SystemTables.getRNew(TT.Contract.Teller.TeAmountLocalOne)
                    R.SDSA.DET.REC<BD.SDSA.CURR.NO> = 1
                    R.SDSA.DET.REC<BD.SDSA.INPUTTER> = EB.SystemTables.getRNew(TT.Contract.Teller.TeInputter)
                    R.SDSA.DET.REC<BD.SDSA.DATE.TIME> = EB.SystemTables.getRNew(TT.Contract.Teller.TeDateTime)
                    R.SDSA.DET.REC<BD.SDSA.AUTHORISER> = EB.SystemTables.getOperator()
                    R.SDSA.DET.REC<BD.SDSA.CO.CODE> = Y.AC.CO.CODE
                    R.SDSA.DET.REC<BD.SDSA.DEPT.CODE> = EB.SystemTables.getRNew(TT.Contract.Teller.TeDeptCode)
                    EB.DataAccess.FWrite(FN.SDSA.DET,Y.REF.NO,R.SDSA.DET.REC)
                    EB.TransactionControl.JournalUpdate(Y.REF.NO)
                
*                    WRITE.FILE.VAR = 'FROM 451 Y.DR.AC: ':Y.DR.AC:' Y.DR.AC.REF: ':Y.DR.AC.REF:' R.SDSA.REF.REC: ':R.SDSA.REF.REC:' R.SDSA.DET.REC: ':R.SDSA.DET.REC:' Y.AC.CO.CODE: ':Y.AC.CO.CODE
*                    GOSUB FILE.WRITE
                END ELSE
                    IF R.SDSA.DET.REC THEN

                        Y.SDSA.ORG.CNT = DCOUNT(R.SDSA.DET.REC<BD.SDSA.ORG.TRANS.REF.NO>,@VM) + 1
                        R.SDSA.DET.REC<BD.SDSA.ORG.TRANS.REF.NO,Y.SDSA.ORG.CNT> = EB.SystemTables.getIdNew()
                        R.SDSA.DET.REC<BD.SDSA.ORG.TRANS.CUR,Y.SDSA.ORG.CNT> = EB.SystemTables.getRNew(TT.Contract.Teller.TeCurrencyOne)
                        R.SDSA.DET.REC<BD.SDSA.ORG.PARTICULAR,Y.SDSA.ORG.CNT> = EB.SystemTables.getRNew(TT.Contract.Teller.TeNarrativeTwo)
                        R.SDSA.DET.REC<BD.SDSA.ORG.DATE,Y.SDSA.ORG.CNT> = EB.SystemTables.getRNew(TT.Contract.Teller.TeValueDateTwo)
                        R.SDSA.DET.REC<BD.SDSA.ORG.DRCR,Y.SDSA.ORG.CNT> = 'CR'
                        R.SDSA.DET.REC<BD.SDSA.ORG.AMT,Y.SDSA.ORG.CNT> = EB.SystemTables.getRNew(TT.Contract.Teller.TeAmountLocalOne)
                        Y.TOT.ORG.AMT = R.SDSA.DET.REC<BD.SDSA.TOT.ORG.AMT> + EB.SystemTables.getRNew(TT.Contract.Teller.TeAmountLocalOne)
                        Y.TOT.OUT.AMT = R.SDSA.DET.REC<BD.SDSA.OUTSTANDING.AMT> + EB.SystemTables.getRNew(TT.Contract.Teller.TeAmountLocalOne)
                        R.SDSA.DET.REC<BD.SDSA.TOT.ORG.AMT> = Y.TOT.ORG.AMT
                        R.SDSA.DET.REC<BD.SDSA.OUTSTANDING.AMT> = Y.TOT.OUT.AMT
                        EB.DataAccess.FWrite(FN.SDSA.DET,Y.REF.NO,R.SDSA.DET.REC)
                        EB.TransactionControl.JournalUpdate(Y.REF.NO)
                    END
*                    WRITE.FILE.VAR = 'FROM 465 Y.DR.AC: ':Y.DR.AC:' Y.DR.AC.REF: ':Y.DR.AC.REF:' R.SDSA.REF.REC: ':R.SDSA.REF.REC:' R.SDSA.DET.REC: ':R.SDSA.DET.REC:' Y.AC.CO.CODE: ':Y.AC.CO.CODE
*                    GOSUB FILE.WRITE
                END
            CASE EB.SystemTables.getRNew(TT.Contract.Teller.TeLocalRef)<1,Y.TT.ORGADJ.POS> EQ 'ADJ' AND EB.SystemTables.getRNew(TT.Contract.Teller.TeLocalRef)<1,Y.TT.AL.POS> EQ 'L'

                IF EB.SystemTables.getRNew(TT.Contract.Teller.TeLocalRef)<1,Y.TT.SADN.POS> NE '' THEN
                    Y.REF.NO = EB.SystemTables.getRNew(TT.Contract.Teller.TeLocalRef)<1,Y.TT.SADN.POS>

                    EB.DataAccess.FRead(FN.SDSA.DET,Y.REF.NO,R.SDSA.DET.REC,F.SDSA.DET,Y.SDSA.DET.ERR)
                    IF R.SDSA.DET.REC THEN
                        Y.SDSA.ADJ.CNT = DCOUNT(R.SDSA.DET.REC<BD.SDSA.ADJ.DATE>,@VM) + 1
                        Y.TOT.ADJ.AMT = R.SDSA.DET.REC<BD.SDSA.TOT.ADJ.AMT> + EB.SystemTables.getRNew(TT.Contract.Teller.TeAmountLocalOne)
                        Y.TOT.OUT.AMT = R.SDSA.DET.REC<BD.SDSA.OUTSTANDING.AMT> - EB.SystemTables.getRNew(TT.Contract.Teller.TeAmountLocalOne)
                        R.SDSA.DET.REC<BD.SDSA.ADJ.TRANS.REF.NO,Y.SDSA.ADJ.CNT> = EB.SystemTables.getIdNew()
                        R.SDSA.DET.REC<BD.SDSA.ADJ.TRANS.CUR,Y.SDSA.ADJ.CNT> = EB.SystemTables.getRNew(TT.Contract.Teller.TeCurrencyOne)
                        R.SDSA.DET.REC<BD.SDSA.ADJ.PARTICULAR,Y.SDSA.ADJ.CNT> = EB.SystemTables.getRNew(TT.Contract.Teller.TeNarrativeTwo)
                        R.SDSA.DET.REC<BD.SDSA.ADJ.DATE,Y.SDSA.ADJ.CNT> = EB.SystemTables.getRNew(TT.Contract.Teller.TeValueDateTwo)
                        R.SDSA.DET.REC<BD.SDSA.ADJ.DRCR,Y.SDSA.ADJ.CNT> = 'DR'
                        R.SDSA.DET.REC<BD.SDSA.ADJ.AMT,Y.SDSA.ADJ.CNT> = EB.SystemTables.getRNew(TT.Contract.Teller.TeAmountLocalOne)
                        R.SDSA.DET.REC<BD.SDSA.TOT.ADJ.AMT> = Y.TOT.ADJ.AMT
                        R.SDSA.DET.REC<BD.SDSA.OUTSTANDING.AMT> = Y.TOT.OUT.AMT
                        R.SDSA.DET.REC<BD.SDSA.CURR.NO> += 1
                        R.SDSA.DET.REC<BD.SDSA.INPUTTER> = EB.SystemTables.getRNew(TT.Contract.Teller.TeInputter)
                        R.SDSA.DET.REC<BD.SDSA.DATE.TIME> = EB.SystemTables.getRNew(TT.Contract.Teller.TeDateTime)
                        R.SDSA.DET.REC<BD.SDSA.AUTHORISER> = EB.SystemTables.getOperator()
                        R.SDSA.DET.REC<BD.SDSA.DEPT.CODE> = EB.SystemTables.getRNew(TT.Contract.Teller.TeDeptCode)
                        EB.DataAccess.FWrite(FN.SDSA.DET,Y.REF.NO,R.SDSA.DET.REC)
                        EB.TransactionControl.JournalUpdate(Y.REF.NO)
                    END
                END
*                WRITE.FILE.VAR = 'FROM 495 Y.DR.AC: ':Y.DR.AC:' Y.DR.AC.REF: ':Y.DR.AC.REF:' R.SDSA.REF.REC: ':R.SDSA.REF.REC:' R.SDSA.DET.REC: ':R.SDSA.DET.REC:' Y.AC.CO.CODE: ':Y.AC.CO.CODE:' Y.REF.NO: ':Y.REF.NO
*                GOSUB FILE.WRITE
        END CASE
        RETURN
*** </region>

*FILE.WRITE:
*        Y.LOG.FILE='sdsa.txt'
*        Y.FILE.DIR ='./DL.BP'
*        OPENSEQ Y.FILE.DIR,Y.LOG.FILE TO F.FILE.DIR ELSE NULL
*        WRITESEQ WRITE.FILE.VAR APPEND TO F.FILE.DIR ELSE NULL
*        CLOSESEQ F.FILE.DIR
*        RETURN

    END
