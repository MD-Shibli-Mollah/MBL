* @ValidationCode : Mjo5NTc5MjM2OTg6Q3AxMjUyOjE2Mjk2MTY2OTM1MDY6dXNlcjotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDE3MTAuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 22 Aug 2021 13:18:13
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0
SUBROUTINE CM.MBL.I.DSP.TR

*-----------------------------------------------------------------------------
*Routine Attach To: Input routine in VERSION.CONTROL of FUNDS.TRANSFER & DRAWINGS
* Modification History :
* Author :      MD. Farid Hossain -- FDS BD
* Reviewed By:  MD. Shibli Mollah -- FDS BD
*-----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.MBL.VER.TR.DISPO
    $INSERT I_F.MBL.CUS.PRIV.TABLE
    
    $USING EB.SystemTables
    $USING FT.Contract
    $USING EB.DataAccess
    $USING EB.OverrideProcessing
    $USING ST.CurrencyConfig
    $USING LC.Contract
* $USING AC.AccountOpening
    
*----------------------------------------INITIALIZE---------------------------------------------------*

    FN.TEMP = 'F.MBL.VER.TR.DISPO' ; F.TEMP = ''
    FN.TEMP.CUS = 'F.MBL.CUS.PRIV.TABLE' ; F.TEMP.CUS = ''
    
*  FN.ACC = 'F.ACCOUNT' ; F.ACC = ''
    FN.CURR = 'F.CURRENCY' ; F.CURR = ''
    FN.FT = 'F.FUNDS.TRANSFER' ; F.FT = ''
    FN.DR = 'F.DRAWINGS' ; F.DR = ''
    
    Y.APP.NAME = EB.SystemTables.getApplication()
    Y.VERSION.NAME.1 = EB.SystemTables.getPgmVersion()
    Y.VERSION.NAME = Y.APP.NAME:Y.VERSION.NAME.1
    Y.OVERR.ID = 'EB-MBL.TREASURY.RATE'
    
*----------------------------------------OPEN.FILES---------------------------------------------------*

    EB.DataAccess.Opf(FN.TEMP, F.TEMP)
    EB.DataAccess.Opf(FN.TEMP.CUS, F.TEMP.CUS)
*   EB.DataAccess.Opf(FN.ACC, F.ACC)
    EB.DataAccess.Opf(FN.CURR, F.CURR)
    EB.DataAccess.Opf(FN.FT, F.FT)
    EB.DataAccess.Opf(FN.DR, F.DR)
    
*----------------------------------------TEMPLATE---------------------------------------------------*

    EB.DataAccess.FRead(FN.TEMP, Y.VERSION.NAME, REC.TEMP, F.TEMP, ERR.TEMP)
    
*-------------------TRACER--------------------------------------------------------------------*
    WriteData = 'INIT TEMPLATE CHECKING---VERSION.NAME: ':Y.VERSION.NAME:' VERSION.REC.TEMP: ':REC.TEMP
    FileName = 'SHIBLI_TREASURY_VER_NAME_AUG_21.txt'
    FilePath = 'DL.BP'
    OPENSEQ FilePath,FileName TO FileOutput THEN NULL
    ELSE
        CREATE FileOutput ELSE
        END
    END
    WRITESEQ WriteData APPEND TO FileOutput ELSE
        CLOSESEQ FileOutput
    END
    CLOSESEQ FileOutput
*----------------------------------------END---------------------------------------------------*
    
    
    IF REC.TEMP EQ '' THEN
        RETURN
    END
    
    Y.FLD.NM = REC.TEMP<MBL.DSP.FIELD.NAME>
*** ADDITIONAL SPREAD --------------------------------------
    Y.TEMP.SPREAD = REC.TEMP<MBL.DSP.ADDITIONAL.SPREAD>
    Y.TEMP.CURR.MAR = REC.TEMP<MBL.DSP.CURRENCY.MARKET>
    
*----------------------------------------VERSION---------------------------------------------------*

    IF Y.APP.NAME EQ 'FUNDS.TRANSFER' THEN
*--------BASE.CURRENCY is required to be considered by MBL-----------------------
        Y.VER.CURR = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.BaseCurrency)
        Y.DEBIT.ACC = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitAcctNo)
        Y.DEBIT.CUR = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitCurrency)
        Y.CREDIT.CUR = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.CreditCurrency)
        
        IF Y.DEBIT.CUR NE 'BDT' THEN
            Y.CR.CUS = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.CreditCustomer)
        END
        IF Y.DEBIT.CUR EQ 'BDT' THEN
            Y.DR.CUS = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitCustomer)
        END
        
*---------------CUSTOMER ID FROM ACC--------------------------------------------------------------------*
*        EB.DataAccess.FRead(FN.ACC, Y.ACC, REC.ACC, F.ACC, ERR.ACC)
*        Y.DEBIT.CUS.ID = REC.ACC<AC.AccountOpening.Account.Customer>
        
        IF Y.FLD.NM EQ 'CUSTOMER.RATE' THEN
            Y.USR.RATE = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.CustomerRate)
        END
*------------if customer rate missing then treasury rate will be picked**************
        ELSE
            Y.USR.RATE = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.TreasuryRate)
        END
*----------For debit currency is FC and credit currency is BDT---------------------------
    
    END
*----------------------------CHECK DRAWINGS-----------------------------------------------------
    IF Y.APP.NAME EQ 'DRAWINGS' THEN
        Y.VER.CURR = EB.SystemTables.getRNew(LC.Contract.Drawings.TfDrDrawCurrency)

        IF Y.FLD.NM EQ 'TREASURY.RATE' THEN
            Y.USR.RATE = EB.SystemTables.getRNew(LC.Contract.Drawings.TfDrTreasuryRate)
        END
*----NEW CONSIDERATION ------for drawings PLZ consider CREDIT.CUST.RATE, RATE.BOOKED, DEBIT.CUST.RATE-------------------
        IF Y.FLD.NM EQ 'CREDIT.CUST.RATE' THEN
            Y.USR.RATE = EB.SystemTables.getRNew(LC.Contract.Drawings.TfDrCreditCustRate)
        END
        IF Y.FLD.NM EQ 'RATE.BOOKED' THEN
            Y.USR.RATE = EB.SystemTables.getRNew(LC.Contract.Drawings.TfDrRateBooked)
        END
        IF Y.FLD.NM EQ 'DEBIT.CUST.RATE' THEN
            Y.USR.RATE = EB.SystemTables.getRNew(LC.Contract.Drawings.TfDrDebitCustRate)
        END
    END
*----------------------------END-----------------------------------------------------
*----------------------------------------CURRENCY/CURRENCY MARKET---------------------------------------------------*

    EB.DataAccess.FRead(FN.CURR, Y.VER.CURR, REC.CURR, F.CURR, ERR.CUR)
    Y.ALL.CURR.MAR = REC.CURR<ST.CurrencyConfig.Currency.EbCurCurrencyMarket>
    Y.ALL.SYS.RATE = REC.CURR<ST.CurrencyConfig.Currency.EbCurMidRevalRate>
    Y.ALL.CURR.MAR.CNT = DCOUNT(Y.ALL.CURR.MAR, @VM)
    
    FOR I = 1 TO Y.ALL.CURR.MAR.CNT
        
*-------------RATES FIXED---FOR CUS PRIV----------------------------
        IF Y.ALL.CURR.MAR<1,I> EQ '13' THEN
            Y.TT.CLEAN = Y.ALL.SYS.RATE<1,I>
        END
        
        IF Y.ALL.CURR.MAR<1,I> EQ '11' THEN
            Y.TT.OD.SELL = Y.ALL.SYS.RATE<1,I>
        END
*-------------------END----------------------------------------------
        
        IF Y.TEMP.CURR.MAR EQ Y.ALL.CURR.MAR<1,I> THEN
            Y.SYS.RATE = Y.ALL.SYS.RATE<1,I>
            BREAK
        END
    
    NEXT I
    
*------------------------CUS PRIV TABLE - FT-CHECK-------------------------------------------------------------------*
*-----------------TT Clean-------------------------------------------------------------------------------------------*
    IF Y.DEBIT.CUR NE 'BDT' AND Y.USR.RATE GT Y.TT.CLEAN THEN
        
        Y.CUS.RATE = Y.USR.RATE
        EB.DataAccess.FRead(FN.TEMP.CUS, Y.CR.CUS, REC.TEMP.CUS, F.TEMP.CUS, ERR.TEMP.CUS)
        IF REC.TEMP.CUS EQ '' THEN
            EB.SystemTables.setText(Y.OVERR.ID)
            Y.OVERRIDE.VAL = EB.SystemTables.getRNew(V-9)
            Y.OVRRD.NO = DCOUNT(Y.OVERRIDE.VAL,@VM) + 1
            EB.OverrideProcessing.StoreOverride(Y.OVRRD.NO)
            RETURN
        END
    
        Y.CUS.TT.CLEAN.SPREAD = REC.TEMP.CUS<MBL.CUS.PRIV.TT.CLEAN.SPREAD>
*Y.TT.CLEAN
        IF Y.CUS.TT.CLEAN.SPREAD NE '' THEN
            IF Y.CUS.TT.CLEAN.SPREAD[1,1] EQ '+' THEN
                Y.CUS.RATE = Y.CUS.RATE + Y.CUS.TT.CLEAN.SPREAD[2, 10]
            END
            ELSE
                Y.CUS.RATE = Y.CUS.RATE - Y.CUS.TT.CLEAN.SPREAD[2, 10]
            END
        END

        IF Y.CUS.RATE NE Y.TT.CLEAN THEN
            EB.SystemTables.setText(Y.OVERR.ID)
            Y.OVERRIDE.VAL = EB.SystemTables.getRNew(V-9)
            Y.OVRRD.NO = DCOUNT(Y.OVERRIDE.VAL,@VM) + 1
            EB.OverrideProcessing.StoreOverride(Y.OVRRD.NO)
            RETURN
        END
    
        IF Y.CUS.RATE EQ Y.TT.CLEAN THEN
            RETURN
        END
        
    END
*-----------------------TT CLEAN --END-------------------------------------------------*

*-------------------TRACER--------------------------------------------------------------------*

    WriteData = 'TT CLEAN --END- ,,USR.RATE: ':Y.CUS.RATE:' Y.CUS.TT.CLEAN.SPREAD: ':Y.CUS.TT.CLEAN.SPREAD:' Y.TT.CLEAN: ':Y.TT.CLEAN:' Y.CR.CUS: ':Y.CR.CUS:' Y.DEBIT.CUR: ':Y.DEBIT.CUR
    FileName = 'SHIBLI_TT.CLEAN.NOT.TRIGG.txt'
    FilePath = 'DL.BP'
    OPENSEQ FilePath,FileName TO FileOutput THEN NULL
    ELSE
        CREATE FileOutput ELSE
        END
    END
    WRITESEQ WriteData APPEND TO FileOutput ELSE
        CLOSESEQ FileOutput
    END
    CLOSESEQ FileOutput
*----------------------------------------END---------------------------------------------------*


*-----------------TT & OD Selling -----------------------------------------------------
    IF Y.DEBIT.CUR EQ 'BDT' AND  Y.USR.RATE LT Y.TT.OD.SELL THEN
        Y.CUS.RATE = Y.USR.RATE
        EB.DataAccess.FRead(FN.TEMP.CUS, Y.DR.CUS, REC.TEMP.CUS, F.TEMP.CUS, ERR.TEMP.CUS)

        IF REC.TEMP.CUS EQ '' THEN
            EB.SystemTables.setText(Y.OVERR.ID)
            Y.OVERRIDE.VAL = EB.SystemTables.getRNew(V-9)
            Y.OVRRD.NO = DCOUNT(Y.OVERRIDE.VAL,@VM) + 1
            EB.OverrideProcessing.StoreOverride(Y.OVRRD.NO)
            RETURN
        END
        Y.CUS.TT.OD.SELL.SPREAD = REC.TEMP.CUS<MBL.CUS.PRIV.BC.SELLING.SPREAD>
*Y.TT.OD.SELL
        IF Y.CUS.TT.OD.SELL.SPREAD NE '' THEN
            IF Y.CUS.TT.OD.SELL.SPREAD[1,1] EQ '+' THEN
                Y.CUS.RATE = Y.CUS.RATE + Y.CUS.TT.OD.SELL.SPREAD[2, 10]
            END
            ELSE
                Y.CUS.RATE = Y.CUS.RATE - Y.CUS.TT.OD.SELL.SPREAD[2, 10]
            END
        END
*-------------------TRACER--------------------------------------------------------------------*

        WriteData = '-INSIDE-TT & OD Selling--,,,USR.RATE: ':Y.CUS.RATE:' Y.CUS.TT.OD.SELL.SPREAD: ':Y.CUS.TT.OD.SELL.SPREAD:' Y.TT.OD.SELL: ':Y.TT.OD.SELL:' Y.DR.CUS: ':Y.DR.CUS:' Y.DEBIT.CUR: ':Y.DEBIT.CUR
        FileName = 'SHIBLI_TT.OD.SELL_AUG_21_FINAL.txt'
        FilePath = 'DL.BP'
        OPENSEQ FilePath,FileName TO FileOutput THEN NULL
        ELSE
            CREATE FileOutput ELSE
            END
        END
        WRITESEQ WriteData APPEND TO FileOutput ELSE
            CLOSESEQ FileOutput
        END
        CLOSESEQ FileOutput
*----------------------------------------END---------------------------------------------------*
        IF Y.CUS.RATE NE Y.TT.OD.SELL THEN
            EB.SystemTables.setText(Y.OVERR.ID)
            Y.OVERRIDE.VAL = EB.SystemTables.getRNew(V-9)
            Y.OVRRD.NO = DCOUNT(Y.OVERRIDE.VAL,@VM) + 1
            EB.OverrideProcessing.StoreOverride(Y.OVRRD.NO)
            RETURN
        END

        IF Y.CUS.RATE EQ Y.TT.OD.SELL THEN
            RETURN
        END
    END
    
*--------------------------------------BASE TEMPLATE--SPREAD CALCULATION---------------------------------------------------*
    IF Y.TEMP.SPREAD NE '' THEN
        IF Y.TEMP.SPREAD[1,1] EQ '+' THEN
            Y.SYS.RATE = Y.SYS.RATE + Y.TEMP.SPREAD[2, 10]
        END
        ELSE
            Y.SYS.RATE = Y.SYS.RATE - Y.TEMP.SPREAD[2, 10]
        END
    END
*----------------------------------------OPERATION-------------------------------------------------------------------------*
    
    IF Y.USR.RATE NE Y.SYS.RATE THEN
        EB.SystemTables.setText(Y.OVERR.ID)
        Y.OVERRIDE.VAL = EB.SystemTables.getRNew(V-9)
        Y.OVRRD.NO = DCOUNT(Y.OVERRIDE.VAL,@VM) + 1
        EB.OverrideProcessing.StoreOverride(Y.OVRRD.NO)
    END
*-------------------TRACER--------------------------------------------------------------------*

    WriteData = 'AFTER ALL(CUS TEMPLATE) CHECKING---USR.RATE: ':Y.USR.RATE:' Y.CUS.TT.CLEAN.SPREAD: ':Y.CUS.TT.CLEAN.SPREAD:' Y.TT.CLEAN: ':Y.TT.CLEAN:' Y.CR.CUS: ':Y.CR.CUS:' Y.DEBIT.CUR: ':Y.DEBIT.CUR:' Y.SYS.RATE: ':Y.SYS.RATE
    FileName = 'SHIBLI_TREASURY_AUG_21.txt'
    FilePath = 'DL.BP'
    OPENSEQ FilePath,FileName TO FileOutput THEN NULL
    ELSE
        CREATE FileOutput ELSE
        END
    END
    WRITESEQ WriteData APPEND TO FileOutput ELSE
        CLOSESEQ FileOutput
    END
    CLOSESEQ FileOutput
*----------------------------------------END---------------------------------------------------*
RETURN
END