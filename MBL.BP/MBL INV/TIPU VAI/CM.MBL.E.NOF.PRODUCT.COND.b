* @ValidationCode : MjotMTczNTE1NTk3OTpDcDEyNTI6MTYwNjgyNDkwMjQyODp1c2VyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMTcxMC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 01 Dec 2020 18:15:02
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE CM.MBL.E.NOF.PRODUCT.COND(Y.RETURN)
     
* Modification History :
*-----------------------------------------------------------------------------
*  Modified by MD SHIBLI MOLLAH FDS -- on 1ST DEC 2020
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
    $USING AA.ProductFramework
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
    
    LOCATE "PRODUCT.ID" IN EB.Reports.getEnqSelection()<2,1> SETTING CUSTOMER.POS THEN
        Y.CUS.ID = EB.Reports.getEnqSelection()<4,CUSTOMER.POS>
    END
    
    FN.AA.PRD.DES.INTEREST = "F.AA.PRD.DES.INTEREST"
    F.AA.PRD.DES.INTEREST = ""
    
    FN.AA.PRODUCT="F.AA.PRODUCT"
    F.AA.PRODUCT=""
    
    FN.AA.PRODUCT.DESIGNER = "F.AA.PRODUCT.DESIGNER"
    F.AA.PRODUCT.DESIGNER = ""
    
    FN.AA.PROPERTY = "F.AA.PROPERTY"
    F.AA.PROPERTY = ""
    
    FN.AA.PRODUCT.GROUP = "F.AA.PRODUCT.GROUP"
    F.AA.PRODUCT.GROUP = ""
    
    FN.AA.PRODUCT.LINE = "F.AA.PRODUCT.LINE"
    F.AA.PRODUCT.LINE = ""
    
    FN.ACC="F.ACCOUNT"
    F.ACC=""
    
    FN.COM="F.COMPANY"
    F.COM=""
    
    FN.LIMIT="F.LIMIT"
    F.LIMIT=""
    
    FN.AA.ARRANGEMENT ="F.AA.ARRANGEMENT"
    F.AA.ARRANGEMENT= ""
    
    FN.CAT="F.CATEGORY"
    F.CAT=""

RETURN
*============*
OPNFILE:
*============*
    EB.DataAccess.Opf(FN.AA.ARRANGEMENT, F.AA.ARRANGEMENT)
    EB.DataAccess.Opf(FN.AA.PRODUCT,F.AA.PRODUCT)
    EB.DataAccess.Opf(FN.AA.PRODUCT.GROUP,F.AA.PRODUCT.GROUP)
    EB.DataAccess.Opf(FN.AA.PROPERTY,F.AA.PROPERTY)
    EB.DataAccess.Opf(FN.AA.PRODUCT.LINE,F.AA.PRODUCT.LINE)
    EB.DataAccess.Opf(FN.AA.PRODUCT.DESIGNER,F.AA.PRODUCT.DESIGNER)
    EB.DataAccess.Opf(FN.ACC, F.ACC)
    EB.DataAccess.Opf(FN.COM,F.COM)
    EB.DataAccess.Opf(FN.LIMIT,F.LIMIT)
    EB.DataAccess.Opf(FN.CAT,F.CAT)

RETURN

PROCESS:
*
    DEBUG
    SEL.CMD = "SELECT ":FN.AA.PRODUCT.DESIGNER
*
    EB.DataAccess.Readlist(SEL.CMD, SEL.LIST, "",NO.OF.RECORD,RET.CODE)
    LOOP
        REMOVE Y.PRD.ID FROM SEL.LIST SETTING POS
    WHILE Y.PRD.ID:POS
        EB.DataAccess.FRead(FN.AA.PRODUCT.DESIGNER, Y.PRD.ID, R.AA.PRD.D, F.AA.PRODUCT.DESIGNER, ERR.AA.PROD)
        Y.PRODUCT.NAME = R.AA.PRD.D<AA.ProductManagement.ProductDesigner.PrdDescription>
        Y.PRD.GRP = R.AA.PRD.D<AA.ProductManagement.ProductDesigner.PrdProductGroup>
        
* AA.PRD.DESIGNER >> PROPERTY> AA.PROPERTY>> PROPERTY CLASS >> APP(AA.PRD.DES.PROPERTY CLASS) >> COND
        Y.PROP.ALL = R.AA.PRD.D<AA.ProductManagement.ProductDesigner.PrdProperty>
        Y.PRD.PROP.ALL = R.AA.PRD.D<AA.ProductManagement.ProductDesigner.PrdPrdProperty>
        
        
        CONVERT SM TO VM IN Y.PROP.ALL
        CONVERT SM TO VM IN Y.PRD.PROP.ALL
        
        Y.DCOUNT = DCOUNT(Y.PROP.ALL,@VM)
        FOR I = 1 TO Y.DCOUNT
            Y.PROP = Y.PROP.ALL<1,I>
            Y.PROP.ID = Y.PRD.PROP.ALL<1,I>
            
            EB.DataAccess.FRead(FN.AA.PROPERTY, Y.PROP, R.AA.PRP, FN.AA.PROPERTY, ERR.AA.PRP)
            Y.PRP.CLS = R.AA.PRP<AA.ProductFramework.Property.PropPropertyClass>
            
            APP.NAME = "F.AA.PRD.DES.":Y.PRP.CLS
            FN.PRD.DES = APP.NAME
            F.PRD.DES = ""
            
            EB.DataAccess.Opf(FN.PRD.DES,F.PRD.DES)
            
            SEL.CMD.DES = "SELECT ":FN.PRD.DES:" WITH @ID LIKE ":Y.PROP.ID:"..."
            EB.DataAccess.Readlist(SEL.CMD.DES, SEL.LIST.DES, "",NO.OF.REC,RET.CODE.DES)
            LOOP
                REMOVE Y.PRD.DES.ID FROM SEL.LIST.DES SETTING POS
            WHILE Y.PRD.DES.ID:POS
                EB.DataAccess.FRead(FN.PRD.DES, Y.PRD.DES.ID, R.PRD.DES, F.PRD.DES, ERR.AA)
                
            
            REPEAT
        NEXT I
        
*---PRODUCT LINE--------------
        EB.DataAccess.FRead(FN.AA.PRODUCT.GROUP, Y.PRD.GRP, R.AA.PRD.GRP, F.AA.PRODUCT.GROUP, ERR.AA.PRD.GRP)
        Y.PRODUCT.LINE = R.AA.PRD.GRP<AA.ProductFramework.ProductGroup.PgProductLine>
       
        
        
*----INTEREST PRO-------------
        
        
        
        
        
        
        
        
        Y.FIXED.RATE = ''
        
        Y.PERIODIC.INDEX = ''
        
        Y.ACCRUAL.RULE = ''
           
*----------INV ID------------------------
        EB.DataAccess.FRead(FN.LIMIT, Y.LIM.REF, R.ARGMNT, F.LIMIT, LIM.ERR)
        Y.INV.ID = R.ARGMNT<LI.Config.Limit.Account>
*----------------------------------------------
        EB.DataAccess.FRead(FN.AA.ARRANGEMENT, Y.COL.ARR.ID, REC.ARR, F.AA.ARRANGEMENT, ERR.ARR)
        Y.ACC.NO = REC.ARR<AA.Framework.Arrangement.ArrLinkedApplId>
        
        EB.DataAccess.FRead(FN.ACC, Y.ACC.NO, REC.ACCT, F.ACC, ERR)
        Y.AC.BAL = REC.ACCT<AC.AccountOpening.Account.OnlineActualBal>
        Y.AC.CUS = REC.ACCT<AC.AccountOpening.Account.Customer>
     
        
*----------------------------------------------------------------------

        Y.RETURN<-1> = Y.PRODUCT.LINE:"*":Y.PRODUCT.NAME:"*":Y.FIXED.RATE:"*":Y.PERIODIC.INDEX:"*":Y.ACCRUAL.RULE
*                             1                   2                 3                 4                    5
    REPEAT
RETURN
    
END