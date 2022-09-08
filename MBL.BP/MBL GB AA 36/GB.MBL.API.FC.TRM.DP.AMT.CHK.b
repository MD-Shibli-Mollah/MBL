* @ValidationCode : MjoxNzkyMTc1MDAyOkNwMTI1MjoxNjI1NzI0MDc4Njc0OnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 08 Jul 2021 12:01:18
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0
SUBROUTINE GB.MBL.API.FC.TRM.DP.AMT.CHK
*-----------------------------------------------------------------------------
*Subroutine Description: attach api routine for FC term deposit minimum amount validation
*Attached As           :  ROUTINE
*Developed by          : S.M. Sayeed
*Designation           : Technical Consultant
*Email                 : s.m.sayeed@fortress-global.com
*Incoming Parameters   : arrId, arrProp, arrCcy
*Outgoing Parameters   : balanceAmount
*----------------------------------------------------------------------------------------
* Modification History : F.CURRENCY renamed to F1.CURRENCY, SM,VM renamed to @SM,@VM
* 1)
*  Date : 08 JULY 2021
*  Modification Description : F.CURRENCY renamed to F1.CURRENCY, SM,VM renamed to @SM,@VM
*
*  Modified By  : MD SHIBLI MOLLAH - FDS BD
*
*-----------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $USING AA.TermAmount
    $USING AA.Framework
    $USING EB.DataAccess
    $USING EB.SystemTables
    $USING ST.CurrencyConfig
    $USING EB.ErrorProcessing
    
    FN.TERM.AMT = 'F.AA.PRD.DES.TERM.AMOUNT'
    F.TERM.AMT = ''
    FN.CURRENCY = 'F.CURRENCY'
*-------variable renamed--------------
    F1.CURRENCY = ''
    
    EB.DataAccess.Opf(FN.TERM.AMT, F.TERM.AMT)
    EB.DataAccess.Opf(FN.CURRENCY, F1.CURRENCY)
    
    TotalActivityRec = AA.Framework.getC_aalocarractivityrec()
    PrdCurrency = TotalActivityRec<AA.Framework.ArrangementActivity.ArrActCurrency>
    ProductName = TotalActivityRec<AA.Framework.ArrangementActivity.ArrActProduct>

    IF ProductName EQ 'MBL.EXPRETQUOTATD.DP.PR' AND PrdCurrency NE 'USD' THEN
        prdConditionName = 'MBL.EXPRETQUOTATD.DP-USD--...'
        SEL.CMD = 'SELECT ':FN.TERM.AMT:' WITH @ID LIKE ':prdConditionName
        EB.DataAccess.Readlist(SEL.CMD, SEL.LIST, '', NO.OF.REC, SystemReturnCode)
        TermAmtId = SEL.LIST<NO.OF.REC>
        EB.DataAccess.FRead(FN.TERM.AMT, TermAmtId, REC.TERM, F.TERM.AMT, Er1)
        NrAttributeTot = REC.TERM<AA.TermAmount.TermAmount.AmtNrAttribute>
        NrTypeTot = REC.TERM<AA.TermAmount.TermAmount.AmtNrType>
        NrValueTot = REC.TERM<AA.TermAmount.TermAmount.AmtNrValue>
        NoOfNrAttribute = DCOUNT(NrAttributeTot,@VM)
        FOR K = 1 TO NoOfNrAttribute
            IF NrAttributeTot<1,K> EQ 'AMOUNT' THEN
                FOR M = 1 TO K
                    IF NrTypeTot<1,K,M> EQ 'MINIMUM' THEN
                        MinimumAmount = NrValueTot<1,K,M>
                        BREAK
                    END
                NEXT M
                BREAK
            END
        NEXT K
        EB.DataAccess.FRead(FN.CURRENCY, 'USD', REC.CUR, F1.CURRENCY, Er2)
        TotCurrencyMarket = REC.CUR<ST.CurrencyConfig.Currency.EbCurCurrencyMarket>
        TotMidLevelRate = REC.CUR<ST.CurrencyConfig.Currency.EbCurMidRevalRate>
        CONVERT @SM TO @VM IN TotCurrencyMarket
        CONVERT @SM TO @VM IN TotMidLevelRate
        totnoOfmarket = DCOUNT(TotCurrencyMarket,@VM)
        FOR I = 1 TO totnoOfmarket
            exactCurrency = TotCurrencyMarket<1,I>
            IF exactCurrency EQ 1 THEN
                positioncur = I
                MidLevelRate = TotMidLevelRate<1,I>
                BREAK
            END
        NEXT I
        BdtAmountUsd = MidLevelRate * MinimumAmount
        EB.DataAccess.FRead(FN.CURRENCY, PrdCurrency, REC.CUR2, F1.CURRENCY, Er2)
        TotCurrencyMarket2 = REC.CUR2<ST.CurrencyConfig.Currency.EbCurCurrencyMarket>
        TotMidLevelRate2 = REC.CUR2<ST.CurrencyConfig.Currency.EbCurMidRevalRate>
        CONVERT @SM TO @VM IN TotCurrencyMarket2
        totnoOfmarket2 = DCOUNT(TotCurrencyMarket2,@VM)
        FOR J = 1 TO totnoOfmarket2
            exactCurrency2 = TotCurrencyMarket2<1,J>
            IF exactCurrency2 EQ 1 THEN
                positioncur2 = J
                MidLevelRate2 = TotMidLevelRate2<1,J>
                BREAK
            END
        NEXT J
        TermAmountDp = EB.SystemTables.getRNew(AA.TermAmount.TermAmount.AmtAmount)
        BdtAmountPrdcur = TermAmountDp * MidLevelRate2
        IF BdtAmountPrdcur LT BdtAmountUsd THEN
            ErrorMessage = 'Term amonut should be greater or eqaul eqivalent ':MinimumAmount:' USD'
            EB.SystemTables.setEtext(ErrorMessage)
            EB.ErrorProcessing.StoreEndError()
        END
    END
    IF ProductName EQ 'MBL.NONRESFCCYTD.DP.PR' AND PrdCurrency NE 'USD' AND PrdCurrency NE 'GBP' THEN
        prdConditionName = 'MBL.NONRESFCCYTD.DP-USD--...'
        SEL.CMD = 'SELECT ':FN.TERM.AMT:' WITH @ID LIKE ':prdConditionName
        EB.DataAccess.Readlist(SEL.CMD, SEL.LIST, '', NO.OF.REC, SystemReturnCode)
        TermAmtId = SEL.LIST<NO.OF.REC>
        EB.DataAccess.FRead(FN.TERM.AMT, TermAmtId, REC.TERM, F.TERM.AMT, Er1)
        NrAttributeTot = REC.TERM<AA.TermAmount.TermAmount.AmtNrAttribute>
        NrTypeTot = REC.TERM<AA.TermAmount.TermAmount.AmtNrType>
        NrValueTot = REC.TERM<AA.TermAmount.TermAmount.AmtNrValue>
        NoOfNrAttribute = DCOUNT(NrAttributeTot,@VM)
        FOR K = 1 TO NoOfNrAttribute
            IF NrAttributeTot<1,K> EQ 'AMOUNT' THEN
                FOR M = 1 TO K
                    IF NrTypeTot<1,K,M> EQ 'MINIMUM' THEN
                        MinimumAmount = NrValueTot<1,K,M>
                        BREAK
                    END
                NEXT M
                BREAK
            END
        NEXT K
        EB.DataAccess.FRead(FN.CURRENCY, 'USD', REC.CUR, F1.CURRENCY, Er2)
        TotCurrencyMarket = REC.CUR<ST.CurrencyConfig.Currency.EbCurCurrencyMarket>
        TotMidLevelRate = REC.CUR<ST.CurrencyConfig.Currency.EbCurMidRevalRate>
        CONVERT @SM TO @VM IN TotCurrencyMarket
        CONVERT @SM TO @VM IN TotMidLevelRate
        totnoOfmarket = DCOUNT(TotCurrencyMarket,@VM)
        FOR I = 1 TO totnoOfmarket
            exactCurrency = TotCurrencyMarket<1,I>
            IF exactCurrency EQ 1 THEN
                positioncur = I
                MidLevelRate = TotMidLevelRate<1,I>
                BREAK
            END
        NEXT I
        BdtAmountUsd = MidLevelRate * MinimumAmount
        EB.DataAccess.FRead(FN.CURRENCY, PrdCurrency, REC.CUR2, F1.CURRENCY, Er2)
        TotCurrencyMarket2 = REC.CUR2<ST.CurrencyConfig.Currency.EbCurCurrencyMarket>
        TotMidLevelRate2 = REC.CUR2<ST.CurrencyConfig.Currency.EbCurMidRevalRate>
        CONVERT @SM TO @VM IN TotCurrencyMarket2
        totnoOfmarket2 = DCOUNT(TotCurrencyMarket2,@VM)
        FOR J = 1 TO totnoOfmarket2
            exactCurrency2 = TotCurrencyMarket2<1,J>
            IF exactCurrency2 EQ 1 THEN
                positioncur2 = J
                MidLevelRate2 = TotMidLevelRate2<1,J>
                BREAK
            END
        NEXT J
        TermAmountDp = EB.SystemTables.getRNew(AA.TermAmount.TermAmount.AmtAmount)
        BdtAmountPrdcur = TermAmountDp * MidLevelRate2
        IF BdtAmountPrdcur LT BdtAmountUsd THEN
            ErrorMessage = 'Term amonut should be greater or eqaul eqivalent ':MinimumAmount:' USD'
            EB.SystemTables.setEtext(ErrorMessage)
            EB.ErrorProcessing.StoreEndError()
        END
    END
RETURN
END
