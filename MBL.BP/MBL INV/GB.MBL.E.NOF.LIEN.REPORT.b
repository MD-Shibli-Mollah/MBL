* @ValidationCode : MjotMTkyMDQyMDE1NTpDcDEyNTI6MTYyOTExMzIxNDc2MDp1c2VyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMTcxMC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 16 Aug 2021 17:26:54
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE GB.MBL.E.NOF.LIEN.REPORT(Y.RETURN)
     
* Modification History :
*-----------------------------------------------------------------------------
*  Modified by MD SHIBLI MOLLAH FDS -- on 24TH NOV 2020
*-----------------------------------------------------------------------------
    $INSERT  I_COMMON
    $INSERT  I_EQUATE
 
    $USING EB.Reports
    $USING EB.SystemTables
    $USING AC.AccountOpening
    $USING EB.DataAccess
    $USING AA.Framework
    $USING AA.TermAmount
    $USING AA.Account
    $USING AA.ProductManagement
    $USING ST.Customer
    $USING ST.CompanyCreation
    $USING AA.PaymentSchedule
    $USING LI.Config
    $USING CO.Contract
    $USING ST.Config

    GOSUB INTT
    GOSUB OPNFILE
    GOSUB PROCESS
    
RETURN

*============
INTT:
*============
    Y.COMP = EB.SystemTables.getIdCompany()
    
    LOCATE "CUSTOMER" IN EB.Reports.getEnqSelection()<2,1> SETTING CUSTOMER.POS THEN
        Y.CUS.ID = EB.Reports.getEnqSelection()<4,CUSTOMER.POS>
    END
    LOCATE "ACCOUNT.NO" IN EB.Reports.getEnqSelection()<2,1> SETTING ACCOUNT.NO.POS THEN
        Y.ACC.NO = EB.Reports.getEnqSelection()<4,ACCOUNT.NO.POS>
    END
 
    FN.CUS="F.CUSTOMER"
    F.CUS=""
    
    FN.ACC="F.ACCOUNT"
    F.ACC=""
    
    FN.COM="F.COMPANY"
    F.COM=""
    
    FN.COL="F.COLLATERAL"
    F.COL=""
    
    FN.COL.HIS="F.COLLATERAL$HIS"
    F.COL.HIS=""
    
    FN.COL.RIGHT="F.COLLATERAL.RIGHT"
    F.COL.RIGHT=""
    
    FN.LIMIT="F.LIMIT"
    F.LIMIT=""
    
    FN.AA.ARRANGEMENT ="F.AA.ARRANGEMENT"
    F.AA.ARRANGEMENT= ""

    FN.AA.PRODUCT="F.AA.PRODUCT"
    F.AA.PRODUCT=""
    
    FN.AA.PRODUCT="F.AA.PRODUCT"
    F.AA.PRODUCT=""
    
    FN.CAT="F.CATEGORY"
    F.CAT=""
    
    Y.ARR.ID = ""

RETURN
*============*
OPNFILE:
*============*
    EB.DataAccess.Opf(FN.AA.ARRANGEMENT, F.AA.ARRANGEMENT)
    EB.DataAccess.Opf(FN.AA.PRODUCT,F.AA.PRODUCT)
    EB.DataAccess.Opf(FN.COL.HIS,F.COL.HIS)
    EB.DataAccess.Opf(FN.COL.RIGHT,F.COL.RIGHT)
    EB.DataAccess.Opf(FN.ACC, F.ACC)
    EB.DataAccess.Opf(FN.CUS,F.CUS)
    EB.DataAccess.Opf(FN.COM,F.COM)
    EB.DataAccess.Opf(FN.COL,F.COL)
    EB.DataAccess.Opf(FN.LIMIT,F.LIMIT)
    EB.DataAccess.Opf(FN.CAT,F.CAT)

RETURN

PROCESS:
*
*    Y.ACC.LEN = Y.ACC.NO[1,2]
*
*    IF Y.ACC.LEN NE "AA" THEN
*        EB.DataAccess.FRead(FN.ACC, Y.ACC.NO, REC.ACCT, F.ACC, ERR)
*        Y.ACC.NO = REC.ACCT<AC.AccountOpening.Account.ArrangementId>
*    END
*
*    ELSE
*        Y.ACC.NO = Y.ACC.NO
*    END

    IF Y.ACC.NO NE "" THEN
        SEL.CMD.COL = "SELECT ":FN.COL:" WITH APPLICATION.ID EQ ":Y.ACC.NO
    END
    
    IF Y.ACC.NO NE "" AND Y.CUS.ID NE "" THEN
        SEL.CMD.COL = "SELECT ":FN.COL:" WITH APPLICATION.ID EQ ":Y.ACC.NO:"AND @ID LIKE ":Y.CUS.ID:"..."
    END
    
    IF Y.CUS.ID NE '' AND Y.ACC.NO EQ '' THEN
        SEL.CMD.COL = "SELECT ":FN.COL:" WITH @ID LIKE ":Y.CUS.ID:"..."
    END
    
    IF Y.ACC.NO EQ '' AND Y.CUS.ID EQ '' THEN
        SEL.CMD.COL = "SELECT ":FN.COL
    END
*
    
    EB.DataAccess.Readlist(SEL.CMD.COL, SEL.LIST, "",NO.OF.RECORD,RET.CODE)
    LOOP
        REMOVE Y.COL.ID FROM SEL.LIST SETTING POS
    WHILE Y.COL.ID:POS
        EB.DataAccess.FRead(FN.COL, Y.COL.ID, R.COL, F.COL, ERR.COL)
        Y.COL.ARR.ID = R.COL<CO.Contract.Collateral.CollApplicationId>
        Y.CUR.NO = R.COL<CO.Contract.Collateral.CollCurrNo>
        Y.VAL.DATE = R.COL<CO.Contract.Collateral.CollValueDate>
        Y.INPUT = R.COL<CO.Contract.Collateral.CollInputter>
        Y.AUTH = R.COL<CO.Contract.Collateral.CollAuthoriser>

        Y.INPUT = FIELD(Y.INPUT,'_',2)
        Y.AUTH = FIELD(Y.AUTH,'_',2)
        
        Y.COL.R.ID = FIELD(Y.COL.ID,'.',1,2)
        
*---------AA ID --------CONV------------------
        Y.ARR.LEN = Y.COL.ARR.ID[1,2]
    
        IF Y.ARR.LEN NE "AA" THEN
            EB.DataAccess.FRead(FN.ACC, Y.COL.ARR.ID, REC.ACCT, F.ACC, ERR)
            Y.ARR.ID = REC.ACCT<AC.AccountOpening.Account.ArrangementId>
        END
    
        ELSE
            Y.ARR.ID = Y.COL.ARR.ID
        END
    
        
*-----------LIMIT REFERENCE-----------------
        EB.DataAccess.FRead(FN.COL.RIGHT, Y.COL.R.ID, REC.COL.R, F.COL.RIGHT, ERR.ARR2)
        Y.LIM.REF = REC.COL.R<CO.Contract.CollateralRight.CollRightLimitReference>
*----------INV ID------------------------
        EB.DataAccess.FRead(FN.LIMIT, Y.LIM.REF, R.ARGMNT, F.LIMIT, LIM.ERR)
        Y.INV.ID = R.ARGMNT<LI.Config.Limit.Account>
*----------------------------------------------
        EB.DataAccess.FRead(FN.AA.ARRANGEMENT, Y.ARR.ID, REC.ARR, F.AA.ARRANGEMENT, ERR.ARR)
        Y.ACC.NO = REC.ARR<AA.Framework.Arrangement.ArrLinkedApplId>
        Y.CO.CODE = REC.ARR<AA.Framework.Arrangement.ArrCoCode>
        
        EB.DataAccess.FRead(FN.ACC, Y.ACC.NO, REC.ACCT, F.ACC, ERR)
        Y.AC.BAL = REC.ACCT<AC.AccountOpening.Account.OnlineActualBal>
        Y.AC.CUS = REC.ACCT<AC.AccountOpening.Account.Customer>
        
        Y.PRD = REC.ACCT<AC.AccountOpening.Account.Category>
        EB.DataAccess.FRead(FN.CAT, Y.PRD, R.CAT, F.CAT, ERR.C)
        Y.PRD.DES = R.CAT<ST.Config.Category.EbCatShortName>
        
        EB.DataAccess.FRead(FN.CUS, Y.AC.CUS, REC.CUS, F.CUS, ERR.CUS)
        Y.AC.NAME = REC.CUS<ST.Customer.Customer.EbCusNameOne>
*  Y.CO.CODE = REC.CUS<ST.Customer.Customer.EbCusCompanyBook>
    
        Y.PROD.LINE = REC.ARR<AA.Framework.Arrangement.ArrProductLine>

*----------------------------------------------------------------------

        IF Y.COMP EQ Y.CO.CODE AND Y.PROD.LINE EQ 'DEPOSITS' THEN
            Y.RETURN<-1> = Y.ACC.NO:"*":Y.AC.NAME:"*":Y.AC.CUS:"*":Y.PRD.DES:"*":Y.AC.BAL:"*":Y.COL.ID:"*":Y.INPUT:"*":Y.AUTH:"*":Y.VAL.DATE:"*":Y.LIM.REF:"*":Y.INV.ID
        END
    
    REPEAT

RETURN

END