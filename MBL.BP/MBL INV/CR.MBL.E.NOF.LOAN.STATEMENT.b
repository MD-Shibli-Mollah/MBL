* @ValidationCode : MjotNTczNjY4OTQyOkNwMTI1MjoxNjAxMjkyNjUzMDkzOnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 28 Sep 2020 17:30:53
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0
SUBROUTINE CR.MBL.E.NOF.LOAN.STATEMENT(Y.RETURN)
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
*                                                  CREATE   - ENAMUL HAQUE,
*  13/09/2020                                      FDS Bangladesh Limited
*-----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE

    $USING EB.Reports
    $USING EB.Updates
    $USING EB.DataAccess
    $USING EB.SystemTables
    $USING AA.Framework
    $USING ST.CompanyCreation
    $USING AA.Settlement
    $USING AA.Interest
    $USING AA.PaymentSchedule
    $USING AA.TermAmount
    $USING AC.AccountOpening
    $USING ST.Customer
    $USING LI.Config
    $USING AA.Limit
    $USING ST.Config

    GOSUB INIT
    GOSUB OPENFILES
    GOSUB PROCESS
*******
INIT:
*******
    FN.AA = 'F.AA.ARRANGEMENT'
    F.AA = ''
    FN.SET = 'F.AA.ARR.SETTLEMENT'
    F.SET = ''
    FN.AA.AC = 'F.AA.ACCOUNT.DETAILS'
    F.AA.AC = ''
    FN.AA.BILL = 'F.AA.BILL.DETAILS'
    F.AA.BILL= ''
    FN.AA.INT.ACC='F.AA.INTEREST.ACCRUALS'
    F.AA.INT.ACC=''
    FN.CUS='F.CUSTOMER'
    F.CUS=''
    FN.AC='F.ACCOUNT'
    F.AC=''
    
    FN.LIM="F.LIMIT"
    F.LIM=""
    FN.LIMIT="F.AA.ARR.LIMIT"
    F.LIMIT=""
    FN.CATEG="F.CATEGORY"
    F.CATEG=""
    
    Y.CR.OR.DR=''
    Y.NARR=''
    Y.DEBIT.AMOUNT=''
    Y.CREDIT.AMOUNT=''
    LOCATE 'PD.ID' IN EB.Reports.getEnqSelection()<2,1> SETTING PD.ID.POS THEN
        Y.AA.ID = EB.Reports.getEnqSelection()<4,PD.ID.POS>

        RETURN
*******
OPENFILES:
        EB.DataAccess.Opf(FN.AA, F.AA)
        EB.DataAccess.Opf(FN.AA.BILL,F.AA.BILL)
        EB.DataAccess.Opf(FN.AA.AC,F.AA.AC)
        EB.DataAccess.Opf(FN.AA.INT.ACC,F.AA.INT.ACC)
        EB.DataAccess.Opf(FN.CUS,F.CUS)
        EB.DataAccess.Opf(FN.AC,F.AC)
        EB.DataAccess.Opf(FN.CATEG,F.CATEG)
*******
        RETURN
PROCESS:
********
        Y.MNEMONIC = FN.AA.AC[2,3]
        IF Y.MNEMONIC EQ 'BNK' THEN
            Y.CUR.ACC = 'CURACCOUNT'
        END
        IF Y.MNEMONIC EQ 'ISL' THEN
            Y.CUR.ACC = 'CURISACCOUNT'
        END

        EB.DataAccess.FRead(FN.AA, Y.AA.ID, REC.AA, F.AA, E.AA)
        Y.ACC.NUM=REC.AA<AA.Framework.Arrangement.ArrLinkedApplId>
        Y.CUS.NUM=REC.AA<AA.Framework.Arrangement.ArrCustomer>
    
*Regular Outstanding
        Y.TODAY = EB.SystemTables.getToday()
        AA.Framework.GetEcbBalanceAmount(Y.ACC.NUM, Y.CUR.ACC, Y.TODAY, TOT.CUR.AMT, RetError)
        TOT.PR.AMT = TOT.CUR.AMT
*-----------------------------------------------------------------------------------------------------
 
        
        EB.DataAccess.FRead(FN.CUS, Y.CUS.NUM,REC.CUS,F.CUS , E.CUS)
        Y.CUS.NAME=REC.CUS<ST.Customer.Customer.EbCusShortName>
        EB.DataAccess.FRead(FN.AC, Y.ACC.NUM,REC.AC,F.AC , E.AC)
        Y.CATEGORY.ID = REC.AC<AC.AccountOpening.Account.Category>
        
        EB.DataAccess.FRead(FN.CATEG, Y.CATEGORY.ID, REC.CATEG, F.CATEG, ER.CATEG)
        Y.CATEGORY = REC.CATEG<ST.Config.Category.EbCatDescription>
        
        Y.AA.PROP.CL = 'INTEREST'
        AA.Framework.GetArrangementConditions(Y.AA.ID, Y.AA.PROP.CL, PROPERTY, '', RETURN.IDS, RET.VALUES, RET.ERROR)
        R.REC.TA = RAISE(RET.VALUES)
        Y.PROFIT.RATE = R.REC.TA<AA.Interest.Interest.IntEffectiveRate>
*---------------------------------------------------------------------------------------
*Limit Ref.
        SEL.LIMIT.CMD = "SELECT ":FN.LIMIT:" WITH @ID LIKE ":Y.AA.ID:"-":"LIMIT-..."
        EB.DataAccess.Readlist(SEL.LIMIT.CMD, S.LIST,"",NO.OF.REC,R.CODE)
        Y.LN.ID = S.LIST<1,NO.OF.REC>
        EB.DataAccess.FRead(FN.LIMIT, Y.LN.ID, R.ARRNGMNT, F.LIMIT, L.ERR)
        Y.LIMIT.REF= R.ARRNGMNT<AA.Limit.Limit.LimLimitReference>
        Y.LIMIT.SER= R.ARRNGMNT<AA.Limit.Limit.LimLimitSerial>
        Y.LIMIT=Y.LIMIT.REF:".":Y.LIMIT.SER
    
        Y.LN.ID = Y.CUS.NUM:".":"000":Y.LIMIT
        EB.DataAccess.FRead(FN.LIM, Y.LN.ID, REC.LIMIT.ARG, F.LIM, LIM.ERR)
        Y.LIMIT.AMT = REC.LIMIT.ARG<LI.Config.Limit.InternalAmount>
    
*--------------------------------------------------------
        EB.DataAccess.FRead(FN.AA.AC, Y.AA.ID,REC.AA.AC,F.AA.AC , E.AA)
        Y.VALUE.DATE=REC.AA.AC<AA.PaymentSchedule.AccountDetails.AdValueDate>
        Y.TOT.BILL.ID=REC.AA.AC<AA.PaymentSchedule.AccountDetails.AdBillId>
        Y.TOT.BL.STATUS = REC.AA.AC<AA.PaymentSchedule.AccountDetails.AdBillStatus>
        Y.TOT.PAYMT.TYP = REC.AA.AC<AA.PaymentSchedule.AccountDetails.AdPayMethod>
        
        CONVERT SM TO VM IN Y.TOT.BL.STATUS
        CONVERT SM TO VM IN Y.TOT.PAYMT.TYP
        CONVERT SM TO VM IN Y.TOT.BILL.ID
        Y.DCOUNT = DCOUNT(Y.TOT.BILL.ID,@VM)
  
        FOR I=1 TO Y.DCOUNT
            Y.BILL.ID = Y.TOT.BILL.ID<1,I>
            Y.BL.STATUS = Y.TOT.BL.STATUS<1,I>
            Y.PAYMT.TYP = Y.TOT.PAYMT.TYP<1,I>
            
            IF Y.BL.STATUS EQ 'SETTLED' AND Y.PAYMT.TYP EQ 'DUE' THEN
                EB.DataAccess.FRead(FN.BILL.DETAILS, Y.BILL.ID, REC.BILL.DET, F.BILL.DETAILS, ERR.BILL)
                Y.CREDIT.AMOUNT = REC.BILL.DET<AA.PaymentSchedule.BillDetails.BdOrTotalAmount>

                Y.DEBIT.AMOUNT = 0.00
                Y.CR.OR.DR = 'CR'
                Y.NARR = 'REPAYMENT OF OVERDUE'
                
            END
    
            IF Y.BL.STATUS EQ 'UNPAID' AND Y.PAYMT.TYP EQ 'DUE' THEN
                EB.DataAccess.FRead(FN.BILL.DETAILS, Y.BILL.ID, REC.BILL.DET, F.BILL.DETAILS, ERR.BILL)
                Y.DEBIT.AMOUNT = REC.BILL.DET<AA.PaymentSchedule.BillDetails.BdOrTotalAmount>
       
                Y.CREDIT.AMOUNT = 0.00
                Y.CR.OR.DR = 'DR'
                Y.NARR = 'PD RECORD CREATED'
            END
        NEXT I

        Y.RETURN<-1>= Y.CUS.NAME:'*':Y.AA.ID:'*':Y.CATEGORY:'*':Y.LIMIT.AMT:'*':Y.PROFIT.RATE:'*':Y.ACC.NUM:'*':Y.VALUE.DATE:'*':Y.NARR:'*': Y.CR.OR.DR:'*':Y.DEBIT.AMOUNT:'*':Y.CREDIT.AMOUNT:'*':TOT.PR.AMT
               
        RETURN
    END
END