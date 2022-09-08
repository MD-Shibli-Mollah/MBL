* @ValidationCode : MjotMTI5NDQxOTI2MzpDcDEyNTI6MTYyOTExMzU3NTI5Njp1c2VyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMTcxMC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 16 Aug 2021 17:32:55
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
* Developed by MD SHIBLI MOLLAH FDS -- on 07TH OCT
*-----------------------------------------------------------------------------
SUBROUTINE GB.MBL.E.NOF.LIEN.REMOVE(Y.RETURN)

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
    $USING CO.Contract


    GOSUB INTT
    GOSUB OPNFILE
    GOSUB PROCESS
    
RETURN


INTT:
    Y.COMP = EB.SystemTables.getIdCompany()
    LOCATE "CUSTOMER" IN EB.Reports.getEnqSelection()<2,1> SETTING CUSTOMER.POS THEN
        Y.CUS.ID = EB.Reports.getEnqSelection()<4,CUSTOMER.POS>
    END
    LOCATE "ARR.ID" IN EB.Reports.getEnqSelection()<2,1> SETTING ARR.POS THEN
        Y.ARR.ID = EB.Reports.getEnqSelection()<4,ARR.POS>
    END
 
    FN.CUS="F.CUSTOMER"
    F.CUS=""
    
    FN.ACC="F.ACCOUNT"
    F.ACC=""
    
    FN.COM="F.COMPANY"
    F.COM=""
    
*    FN.COL="F.COLLATERAL"
*    F.COL=""
    
    FN.COL.HIS="F.COLLATERAL$HIS"
    F.COL.HIS=""
    
    FN.AA.ARRANGEMENT ="F.AA.ARRANGEMENT"
    F.AA.ARRANGEMENT= ""
    
    FN.PRODUCT='F.AA.PRODUCT'
    F.PRODUCT=''

RETURN
*============*
OPNFILE:
*============*
    EB.DataAccess.Opf(FN.AA.ARRANGEMENT, F.AA.ARRANGEMENT)
    EB.DataAccess.Opf(FN.PRODUCT, F.PRODUCT)
    EB.DataAccess.Opf(FN.COL.HIS,F.COL.HIS)
    EB.DataAccess.Opf(FN.ACC, F.ACC)
    EB.DataAccess.Opf(FN.CUS,F.CUS)
    EB.DataAccess.Opf(FN.COM,F.COM)
*    EB.DataAccess.Opf(FN.COL,F.COL)

RETURN

PROCESS:
    
    IF Y.ARR.ID NE "" THEN
        SEL.CMD.COL = "SELECT ":FN.COL.HIS:" WITH RECORD.STATUS EQ REVE AND APPLICATION.ID EQ ":Y.ARR.ID
    END
    IF Y.ARR.ID NE "" AND Y.CUS.ID NE "" THEN
        SEL.CMD.COL = "SELECT ":FN.COL.HIS:" WITH RECORD.STATUS EQ REVE AND APPLICATION.ID EQ ":Y.ARR.ID:" AND @ID LIKE ":Y.CUS.ID:"..."
    END
    IF Y.CUS.ID NE '' THEN
        SEL.CMD.COL = "SELECT ":FN.COL.HIS:" WITH RECORD.STATUS EQ REVE AND @ID LIKE ":Y.CUS.ID:"..."
    END
    IF Y.CUS.ID EQ '' AND Y.ARR.ID EQ '' THEN
        SEL.CMD.COL = "SELECT ":FN.COL.HIS:" WITH RECORD.STATUS EQ REVE"
    END
 
    EB.DataAccess.Readlist(SEL.CMD.COL, SEL.LIST, "",NO.OF.RECORD,RET.CODE)

    LOOP
        REMOVE Y.COL.HIS.ID FROM SEL.LIST SETTING POS
    WHILE Y.COL.HIS.ID:POS
        EB.DataAccess.FRead(FN.COL.HIS,Y.COL.HIS.ID,REC.HISTORY,F.COL.HIS,ERR)
        Y.CUR.NO = REC.HISTORY<CO.Contract.Collateral.CollCurrNo>
        
        Y.ARR.ID= REC.HISTORY<CO.Contract.Collateral.CollApplicationId>
        Y.DATE= REC.HISTORY<CO.Contract.Collateral.CollDateTime,1>
        Y.DATE2 = Y.DATE[1,6]
        Y.WDR.DATE = '20':Y.DATE2
        Y.INPUT2= REC.HISTORY<CO.Contract.Collateral.CollInputter>
        Y.AUTH2= REC.HISTORY<CO.Contract.Collateral.CollAuthoriser>
        Y.STATUS = REC.HISTORY<CO.Contract.Collateral.CollStatus>

        Y.INPUT21 = FIELD(Y.INPUT2,'_',2)
        Y.AUTH22 = FIELD(Y.AUTH2,'_',2)
 
***************************ALL PREV REC--LIEN, PREV COLUMN***************************************************8
        Y.PREV.CUR = Y.CUR.NO - 1
        Y.COL.HIS.ID.TEMP = FIELD(Y.COL.HIS.ID,';',1)
        Y.PREV.HIS.COL.ID = Y.COL.HIS.ID.TEMP:';':Y.PREV.CUR
        EB.DataAccess.FRead(FN.COL.HIS,Y.PREV.HIS.COL.ID,REC.HISTORY,F.COL.HIS,ERR2)
        Y.INPUT1= REC.HISTORY<CO.Contract.Collateral.CollInputter>
        Y.AUTH1= REC.HISTORY<CO.Contract.Collateral.CollAuthoriser>
        Y.L.DATE = REC.HISTORY<CO.Contract.Collateral.CollDateTime,1>
        Y.DATE1 = Y.L.DATE[1,6]
        Y.LIEN.DATE = '20':Y.DATE1
        Y.INPUT11 = FIELD(Y.INPUT1,'_',2)
        Y.AUTH12 = FIELD(Y.AUTH1,'_',2)
        
*------------------------------CONV ARR ID --------------------------------------------
        Y.ARR.LEN = Y.ARR.ID[1,2]
    
        IF Y.ARR.LEN NE "AA" THEN
            EB.DataAccess.FRead(FN.ACC, Y.ARR.ID, REC.ACCT, F.ACC, ERR)
            Y.ARR.ID = REC.ACCT<AC.AccountOpening.Account.ArrangementId>
        END
    
*--------------------------------------------------------------------------
        EB.DataAccess.FRead(FN.AA.ARRANGEMENT, Y.ARR.ID, REC.ARR, F.AA.ARRANGEMENT, ERR.ARR)
        Y.ACC.NO = REC.ARR<AA.Framework.Arrangement.ArrLinkedApplId>
        Y.PROD.ID = REC.ARR<AA.Framework.Arrangement.ArrProduct>
        Y.CUS.ID = REC.ARR<AA.Framework.Arrangement.ArrCustomer>
        Y.CO.CODE = REC.ARR<AA.Framework.Arrangement.ArrCoCode>
*--------------------------------------------------------------------------
        EB.DataAccess.FRead(FN.PRODUCT, Y.PROD.ID, REC.PROD, F.PRODUCT, ERR.PROD)
        Y.DESC.PROD = REC.PROD<AA.ProductManagement.Product.PdtDescription>
        
        EB.DataAccess.FRead(FN.ACC, Y.ACC.NO, REC.ACCT, F.ACC, ERR)
        Y.AC.BAL = REC.ACCT<AC.AccountOpening.Account.OnlineActualBal>
        Y.AC.CUS = REC.ACCT<AC.AccountOpening.Account.Customer>
        Y.AC.TITLE = REC.ACCT<AC.AccountOpening.Account.AccountTitleOne>
        
        EB.DataAccess.FRead(FN.CUS, Y.AC.CUS, REC.CUS, F.CUS, ERR.CUS)
        
*  Y.CO.CODE = REC.CUS<ST.Customer.Customer.EbCusCompanyBook>
        
*----------------------------------------------------------------------
        IF Y.COMP EQ Y.CO.CODE THEN
            Y.RETURN<-1> = Y.CUS.ID:"*":Y.AC.TITLE:'*':Y.ACC.NO:"*":Y.DESC.PROD:"*":Y.AC.BAL:"*":Y.COL.HIS.ID:"*":Y.INPUT11:"*":Y.AUTH12:"*":Y.LIEN.DATE:"*":Y.STATUS:"*":Y.INPUT21:"*":Y.AUTH22:"*":Y.WDR.DATE
        END
    REPEAT
RETURN
END