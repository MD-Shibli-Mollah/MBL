* @ValidationCode : MjotNDc1MzYzMDI5OkNwMTI1MjoxNjMwNzY0ODkyMDc5OnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 04 Sep 2021 20:14:52
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE GB.MBL.I.CHQ.DATE.VAL
    
*-----------------------------------------------------------------------------
*Subroutine Description: THIS ROUTINE USE FOR VALIDATE CHEQUE DATE NOT GT TODAY AND
* CHEQUE DATE VALIDATION FOR 182 DAYS
*Subroutine Type:
*Attached To    : VERSION.CONTROL(TELLER, FUNDS.TRANSFER)
*Attached As    : INPUT ROUTINE
*-----------------------------------------------------------------------------
* Modification History 1:
* 30/06/2020 -                            Retrofit   - Sarowar Mortoza
*                                                     FDS Pvt Ltd
*-----------------------------------------------------------------------------
* Modification History 2:
* 04/09/2021 -                           MODIFIED BY - MD SHIBLI MOLLAH
*                                                      FDS SERVICES LTD
*-----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $USING FT.Contract
    $USING TT.Contract
    $USING EB.DataAccess
    $USING EB.Updates
    $USING EB.SystemTables
    $USING EB.ErrorProcessing
    $USING EB.API
*-----------------------------------------------------------------------------
    GOSUB INITIALISE ; *
    GOSUB PROCESS ; *
RETURN
*-----------------------------------------------------------------------------

*** <region name= INITIALISE>
INITIALISE:
*** <desc> </desc>
    FN.FT = 'F.FUNDS.TRANSFER'
    F.FT = ''

    FN.TT = 'F.TELLER'
    F.TT = ''
    
    Y.CHQ.ERR = "EB-FT.TT.CHQ.ERR"
    Y.STALE.CHQ = "EB-FT.TT.STALE.CHQ"
    Y.APP = EB.SystemTables.getApplication()
    
    Y.APP.NAME ="TELLER":@FM:"FUNDS.TRANSFER"
    LOCAL.FIELDS = ""
    LOCAL.FIELDS = "LT.AC.CHEQUE.DT":@FM:"LT.AC.CHEQUE.DT"
    FLD.POS = ""
    EB.Updates.MultiGetLocRef(Y.APP.NAME, LOCAL.FIELDS,FLD.POS)
    Y.TT.CHQ.DATE.POS=FLD.POS<1,1>
    Y.FT.CHQ.DATE.POS=FLD.POS<2,1>
    
RETURN
*** </region>


*-----------------------------------------------------------------------------

*** <region name= PROCESS>
PROCESS:
*** <desc> </desc>

    IF Y.APP EQ "TELLER" THEN
        Y.CHQ.DATE = EB.SystemTables.getRNew(TT.Contract.Teller.TeLocalRef)<1,Y.TT.CHQ.DATE.POS>
    END

    IF Y.APP EQ "FUNDS.TRANSFER" THEN
        Y.CHQ.DATE = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.LocalRef)<1,Y.FT.CHQ.DATE.POS>
    END

    Y.TODAY = EB.SystemTables.getToday()
    Y.NO.DAYS = 'C'
    IF Y.CHQ.DATE NE '' AND Y.TODAY NE '' THEN
        EB.API.Cdd('',Y.CHQ.DATE,Y.TODAY,Y.NO.DAYS)
    END
    IF Y.CHQ.DATE NE "" AND Y.CHQ.DATE GT Y.TODAY AND Y.APP EQ "TELLER" THEN
        EB.SystemTables.setAf(TT.Contract.Teller.TeLocalRef)
        EB.SystemTables.setAv(Y.TT.CHQ.DATE.POS)
        EB.SystemTables.setEtext(Y.CHQ.ERR)
        EB.ErrorProcessing.StoreEndError()
    END
    IF Y.CHQ.DATE NE "" AND Y.CHQ.DATE GT Y.TODAY AND Y.APP EQ "FUNDS.TRANSFER" THEN
        EB.SystemTables.setAf(FT.Contract.FundsTransfer.LocalRef)
        EB.SystemTables.setAv(Y.FT.CHQ.DATE.POS)
        EB.SystemTables.setEtext(Y.CHQ.ERR)
        EB.ErrorProcessing.StoreEndError()
    END
    IF Y.CHQ.DATE NE "" AND Y.NO.DAYS GT "182" AND Y.APP EQ "TELLER" THEN
        EB.SystemTables.setAf(TT.Contract.Teller.TeLocalRef)
        EB.SystemTables.setAv(Y.TT.CHQ.DATE.POS)
        EB.SystemTables.setEtext(Y.STALE.CHQ)
        EB.ErrorProcessing.StoreEndError()
    END
    IF Y.CHQ.DATE NE "" AND Y.NO.DAYS GT "182" AND Y.APP EQ "FUNDS.TRANSFER" THEN
        EB.SystemTables.setAf(FT.Contract.FundsTransfer.LocalRef)
        EB.SystemTables.setAv(Y.FT.CHQ.DATE.POS)
        EB.SystemTables.setEtext(Y.STALE.CHQ)
        EB.ErrorProcessing.StoreEndError()
    END

RETURN
*** </region>

END


