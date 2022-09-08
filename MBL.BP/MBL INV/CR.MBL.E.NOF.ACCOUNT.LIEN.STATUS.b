* @ValidationCode : MjotMTQwMjY0ODgzNTpDcDEyNTI6MTYzMDIxNTUxNDk1NTp1c2VyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMTcxMC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 29 Aug 2021 11:38:34
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE CR.MBL.E.NOF.ACCOUNT.LIEN.STATUS(Y.RETURN)
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History 1 :
*-----------------------------------------------------------------------------
* Modified by MD SHIBLI MOLLAH FDS -- on 29TH SEP 2020
*-----------------------------------------------------------------------------
* Modification History 2 :
* 29/08/2021 -                            Developed By   - MD. Shibli Mollah
*                                                     FDS Bangladesh Limited
*------------------------------------------------------------------------------
    $INSERT  I_COMMON
    $INSERT  I_EQUATE
*
    $USING EB.Reports
    $USING EB.SystemTables
    $USING EB.DataAccess
    $USING AA.Framework
    $USING AA.Account
    $USING ST.Customer
    $USING AC.AccountOpening
    $USING AA.ProductManagement
    $USING CO.Contract
    $USING ST.CompanyCreation
*
    GOSUB INIT
    GOSUB OPENFILES
    GOSUB PROCESS
RETURN
*
INIT:
    LOCATE "CUSTOMER" IN EB.Reports.getEnqSelection()<2,1> SETTING CUSTOMER.POS THEN
        Y.CUS.ID=EB.Reports.getEnqSelection()<4,CUSTOMER.POS>
    END
    
    FN.CUS = "F.CUSTOMER"
    F.CUS = ""
*
    FN.ACC = 'F.ACCOUNT'
    F.ACC = ''
*
    FN.AA.ARR = 'F.AA.ARRANGEMENT'
    F.AA.ARR = ''
*
    FN.COLLATERAL = 'F.COLLATERAL'
    F.COLLATERAL = ''
*
    FN.AA.PRODUCT = 'F.AA.PRODUCT'
    F.AA.PRODUCT=''
    
    FN.COMP = 'F.COMPANY'
    F.COMP = ''
     
    Y.COMP = EB.SystemTables.getIdCompany()
*
RETURN
OPENFILES:
    EB.DataAccess.Opf(FN.CUS, F.CUS)
    EB.DataAccess.Opf(FN.ACC,F.ACC)
    EB.DataAccess.Opf(FN.AA.ARR, F.AA.ARR)
    EB.DataAccess.Opf(FN.COLLATERAL, F.COLLATERAL)
    EB.DataAccess.Opf(FN.AA.PRODUCT, F.AA.PRODUCT)
    EB.DataAccess.Opf(FN.COMP, F.COMP)
RETURN
*
PROCESS:
    
*------------------------READ COMPANY----------------------------------------------------
    EB.DataAccess.FRead(FN.COMP, Y.COMP, REC.COMP, F.COMP, Er)
    Y.COMP.NAME = REC.COMP<ST.CompanyCreation.Company.EbComCompanyName>
  
    EB.DataAccess.FRead(FN.CUS, Y.CUS.ID, REC.CUS, F.CUS, ERR.CUS)
    Y.CUS.TITLE = REC.CUS<ST.Customer.Customer.EbCusShortName>
*
    SEL.CMD.COL = "SELECT ":FN.COLLATERAL:" WITH @ID LIKE ":Y.CUS.ID:"..."
    EB.DataAccess.Readlist(SEL.CMD.COL, SEL.LIST, "", LIST.COLL, RET.CODE)

    LOOP
        REMOVE Y.COLL.ID FROM SEL.LIST SETTING POS
    WHILE Y.COLL.ID:POS
*
        EB.DataAccess.FRead(FN.COLLATERAL, Y.COLL.ID, REC.COLL, F.COLLATERAL, ERR.COLL)
        Y.APPL.ID = REC.COLL<CO.Contract.Collateral.CollApplicationId>
        
*----IF ACC Num is in APPL ID--------------------------------------------

        Y.ID.LEN = Y.APPL.ID[1,2]
        IF Y.ID.LEN NE 'AA' THEN
            Y.ACC = Y.APPL.ID
    
            EB.DataAccess.FRead(FN.ACC, Y.ACC, R.ACC, F.ACC, ACC.ERR)
            Y.CUS.ID = R.ACC<AC.AccountOpening.Account.Customer>
            Y.AA.ID = R.ACC<AC.AccountOpening.Account.ArrangementId>
        
            EB.DataAccess.FRead(FN.AA.ARR, Y.AA.ID, R.ARR, F.AA.ARR, ARR.ERR)
            Y.ACCT.NUM = R.ARR<AA.Framework.Arrangement.ArrLinkedApplId>
            Y.PRODUCT = R.ARR<AA.Framework.Arrangement.ArrProduct>
            Y.CUS.ROLE = R.ARR<AA.Framework.Arrangement.ArrCustomerRole>
            Y.PRODUCT.LINE = R.ARR<AA.Framework.Arrangement.ArrProductLine>
        END
        ELSE
            Y.ACCT.NUM = Y.APPL.ID
            Y.ARR.ID = Y.APPL.ID
            EB.DataAccess.FRead(FN.AA.ARR, Y.AA.ID, R.ARR, F.AA.ARR, ARR.ERR)
            Y.PRODUCT = R.ARR<AA.Framework.Arrangement.ArrProduct>
        
        END
*--------------------------------END-------------------------------------------
        
*-----------TOTAL ACCOUNT NUMBER---------------------------
        CONVERT @SM TO @VM IN Y.ACCT.NUM
        Y.TOT.ACC.NUM = DCOUNT(Y.ACCT.NUM,@VM)
*------------------------------------------------------------
        EB.DataAccess.FRead(FN.AA.PRODUCT, Y.PRODUCT, REC.PROD, F.AA.PRODUCT, ERR.PROD)
        Y.DESC.PROD = REC.PROD<AA.ProductManagement.Product.PdtDescription>
        Y.LIEN.REF = Y.COLL.ID
        Y.BALANCE = REC.COLL<CO.Contract.Collateral.CollExecutionValue>
        
        Y.TOT.BALANCE = SUM(Y.BALANCE)
*
        Y.AUTH.NAME = REC.COLL<CO.Contract.Collateral.CollAuthoriser>
        Y.AUTH =FIELD(Y.AUTH.NAME,"_",2)
*
        Y.RETURN<-1> = Y.ACCT.NUM:"*":Y.CUS.TITLE:"*":Y.DESC.PROD:"*":Y.LIEN.REF:"*":Y.BALANCE:"*":Y.AUTH:"*":Y.COLL.DCOUNT:"*":Y.COMP.NAME:"*":Y.TOT.BALANCE
*                           1               2               3               4           5           6               7               8                  9
        
        Y.COLL.ID = ''
        Y.AA.ID = ''
        Y.APPL.ID = ''
        Y.ACCT.NUM = ''
        Y.DESC.PROD = ''
        Y.LIEN.REF = ''
        Y.BALANCE = ''
        Y.AUTH = ''
        Y.COLL.DCOUNT = ''
        Y.TOT.ACC.NUM = ''
        Y.TOT.BALANCE = ''
*
    REPEAT
RETURN
END