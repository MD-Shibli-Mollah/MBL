* @ValidationCode : MjotMzcwMDYyNzAwOkNwMTI1MjoxNjMxNjEwOTQ3NDkxOnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 14 Sep 2021 15:15:47
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0
SUBROUTINE GB.MBL.I.TT.BEARER
 
*-----------------------------------------------------------------------------
*Routine Attach To: Input routine
*Routine Attach Version: TELLER,MBL.LCY.CASHIN
*-----------------------------------------------------------------------------
* Modification History :
*14/04/2021 -                             NEW - MD SHIBLI MOLLAH
*                                                 FDS Bangladesh Limited
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $USING TT.Contract
    $USING FT.Contract
    $USING EB.DataAccess
    $USING EB.SystemTables
    $USING EB.Foundation
    $USING EB.OverrideProcessing
    $USING EB.ErrorProcessing
    $USING AA.Framework
    $USING AC.AccountOpening
*-----------------------------------------------------------------------------
    GOSUB INITIALISE ; *
    GOSUB GET.TRANS.INFO ; *
    GOSUB PROCESS ; *
RETURN
*-----------------------------------------------------------------------------

*** <region name= INITIALISE>
INITIALISE:
*** <desc> </desc>

    Y.TT.CHEQUE.NUMBER = ''
    Y.FT.CHEQUE.NUMBER = ''
    
    FN.AA.ARR = 'F.AA.ARRANGEMENT'
    F.AA.ARR = ''
    EB.DataAccess.Opf(FN.AA.ARR,F.AA.ARR)
    
    FN.AC = 'F.ACCOUNT'
    F.AC = ''
    EB.DataAccess.Opf(FN.AC,F.AC)
    
    AccountId = ''
    Y.PGM.VERSION = ''
    Y.ARR.ID = ''
    Y.ARR.PROD.GRP = ''
    Y.FLAG = ''
    
    
RETURN
*** </region>


*-----------------------------------------------------------------------------

*** <region name= GET.TRANS.INFO>
GET.TRANS.INFO:
    
*** <desc> </desc>
    IF EB.SystemTables.getApplication() EQ 'TELLER' THEN
        Y.TT.CHEQUE.NUMBER= EB.SystemTables.getRNew(TT.Contract.Teller.TeChequeNumber)
        Y.CHEQUE.NUMBER.VAL = ISDIGIT(Y.TT.CHEQUE.NUMBER)
        Y.TXN.AMT=EB.SystemTables.getRNew(TT.Contract.Teller.TeAmountLocalOne)
        AccountId = EB.SystemTables.getRNew(TT.Contract.Teller.TeAccountTwo) ;* Required for SP Account Deposit Validation
    END ELSE
        IF EB.SystemTables.getApplication() EQ 'FUNDS.TRANSFER' THEN
            Y.FT.CHEQUE.NUMBER = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.ChequeNumber)
            Y.CHEQUE.NUMBER.VAL = ISDIGIT(Y.FT.CHEQUE.NUMBER)
        END
    END
RETURN
*** </region>


*-----------------------------------------------------------------------------

*** <region name= PROCESS>
PROCESS:
*** <desc> </desc>
*    IF Y.CHEQUE.NUMBER.VAL EQ '0' THEN
*        EB.SystemTables.setEtext('Cheque Number is Not Numeric')
*        EB.ErrorProcessing.StoreEndError()
*    END
   
    Y.PGM.VERSION = EB.SystemTables.getPgmVersion()
   
    IF Y.PGM.VERSION EQ ',MBL.LCCY.CASHIN' THEN
   
        Y.BEARER.SELF.POS=""
        Y.NAME.BEARER.POS=""
        Y.NID.BEARER.POS=""
        Y.CELL.BEARER.POS=""
        FLD.NAMES = "LT.DRAWN.BY":VM:"LT.TT.BEARER.NM":VM:"LT.TT.NATNL.IDN":VM:"LT.TT.BEARER.CN"
        FLD.POS=""
        Y.APP.NAME ="TELLER"
        EB.Foundation.MapLocalFields(Y.APP.NAME, FLD.NAMES,FLD.POS)
        Y.BEARER.SELF.POS=FLD.POS<1,1>
        Y.NAME.BEARER.POS=FLD.POS<1,2>
        Y.NID.BEARER.POS=FLD.POS<1,3>
        Y.CELL.BEARER.POS=FLD.POS<1,4>
    
        Y.BEARER.SELF = EB.SystemTables.getRNew(TT.Contract.Teller.TeLocalRef)<1,Y.BEARER.SELF.POS>
        Y.NAME.BEARER = EB.SystemTables.getRNew(TT.Contract.Teller.TeLocalRef)<1,Y.NAME.BEARER.POS>
        Y.NID.BEARER = EB.SystemTables.getRNew(TT.Contract.Teller.TeLocalRef)<1,Y.NID.BEARER.POS>
        Y.CELL.BEARER = EB.SystemTables.getRNew(TT.Contract.Teller.TeLocalRef)<1,Y.CELL.BEARER.POS>
    
*    IF Y.BEARER.SELF EQ "Bearer" THEN
        IF Y.BEARER.SELF EQ "Bearer" AND Y.TXN.AMT GT 50000 THEN
*        IF Y.TXN.AMT GT 50000 THEN
            IF (Y.NAME.BEARER EQ '') OR (Y.NID.BEARER EQ '') OR (Y.CELL.BEARER EQ '') THEN
                EB.SystemTables.setEtext('Name/NID/Cell Number of Bearer are Mandatory !!!')
                EB.ErrorProcessing.StoreEndError()
            END
*        END
        END

    END

*---------------------------------------------------------------------
* Validation of Cash Deposit Version for Savings Plan up to Tk. 10000
*---------------------------------------------------------------------
* Added by MD SAGOR ALI, MBL Retail Team
*---------------------------------------------------------------------
        
    EB.DataAccess.FRead(FN.AC, AccountId, AC.REC, F.AC, AC.ERR)
    Y.ARR.ID = AC.REC<AC.AccountOpening.Account.ArrangementId>
    
    EB.DataAccess.FRead(FN.AA.ARR,Y.ARR.ID,ARR.REC,F.ARR,ARR.ERR)
    Y.ARR.PROD.GRP = ARR.REC<AA.Framework.Arrangement.ArrProductGroup>
    
    IF Y.PGM.VERSION EQ ',MBL.LCCY.SP.CASHIN' THEN
        IF Y.TXN.AMT GT 10000 THEN
            EB.SystemTables.setAf(TT.Contract.Teller.TeAmountLocalOne)
            EB.SystemTables.setEtext('Transaction Amount is More than Tk.10,000 !!!')
            EB.ErrorProcessing.StoreEndError()
        END
    
        IF Y.ARR.PROD.GRP EQ 'MBL.SP.GRP.DP' OR Y.ARR.PROD.GRP EQ 'IS.MBL.MMSP.DP' THEN
            Y.FLAG = 'YES'
        END
                
        IF Y.FLAG NE 'YES' THEN
            EB.SystemTables.setAf(TT.Contract.Teller.TeAccountTwo)
            EB.SystemTables.setEtext('Only Savings Plan Deposit Account is Allowed !!!')
            EB.ErrorProcessing.StoreEndError()
        END
    
    END

    
RETURN
*** </region>

END
