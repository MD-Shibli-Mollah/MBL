SUBROUTINE GB.MBL.I.CHEQUE.NO.VALIDATE
*-----------------------------------------------------------------------------
*Routine Attach To: Input routine
*Routine Attach Version: FUNDS.TRANSFER,MBL.IW.CLG
*-----------------------------------------------------------------------------
* Modification History :
*29/03/2020 -                             Retrofit   -MD.SAROWAR MORTOZA
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
    IF Y.CHEQUE.NUMBER.VAL EQ '0' THEN
        EB.SystemTables.setEtext('Cheque Number is Not Numeric')
        EB.ErrorProcessing.StoreEndError()
    END
   
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
    
RETURN
*** </region>

END



