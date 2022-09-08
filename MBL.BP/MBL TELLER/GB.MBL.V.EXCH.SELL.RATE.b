* @ValidationCode : MjoyMDI1OTk1MjYwOkNwMTI1MjoxNjA3NDA2NTUxNDM5OnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 08 Dec 2020 11:49:11
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0
SUBROUTINE GB.MBL.V.EXCH.SELL.RATE

* Modification History :
*-----------------------------------------------------------------------------
* Modified by MD SHIBLI MOLLAH FDS -- on 06TH DEC 2020
* THIS ROUTINE RETURNS THE SELL EXCHANGE RATE FOR TELLER,MBL.SELL.FCY.LACCT
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $USING ST.CurrencyConfig
    $USING EB.SystemTables
    $USING TT.Contract
    $USING EB.DataAccess
    
    GOSUB INIT
    GOSUB OPENFILES
    GOSUB PROCESS
RETURN

*********************
INIT:
********************

    FN.CURR = "F.CURRENCY"
    F.CURR = ""

    FN.TELLER = "F.TELLER"
    F.TELLER = ""


RETURN

********************
OPENFILES:
    EB.DataAccess.Opf(FN.CURR,F.CURR)

RETURN

*****************
PROCESS:
*****************

    Y.CURR = EB.SystemTables.getRNew(TT.Contract.Teller.TeCurrencyOne)

    EB.DataAccess.FRead(FN.CURR, Y.CURR, R.CURR, F.CURR, CURR.ERR)

    Y.CURR.MARKET.ALL = R.CURR<ST.CurrencyConfig.Currency.EbCurCurrencyMarket>
    Y.SELL.RATE.ALL = R.CURR<ST.CurrencyConfig.Currency.EbCurSellRate>

    CONVERT SM TO VM IN Y.CURR.MARKET.ALL

    Y.DCOUNT = DCOUNT(Y.CURR.MARKET.ALL,@VM)

    FOR I=1 TO Y.DCOUNT
        Y.CURR.MARKET = Y.CURR.MARKET.ALL<1,I>

        IF Y.CURR.MARKET EQ '10' THEN
            Y.SELL.RATE = Y.SELL.RATE.ALL<1,I>
        END
    NEXT I

    EXCH.RATE = Y.SELL.RATE

    EB.SystemTables.setRNew(TT.Contract.Teller.TeDealRate, EXCH.RATE)

RETURN
END