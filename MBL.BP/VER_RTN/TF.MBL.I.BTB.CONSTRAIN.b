* @ValidationCode : MjoxNjk5Mzg2Mjg1OkNwMTI1MjoxNTczMDI1NTA5MjE0OkRFTEw6LTE6LTE6MDowOmZhbHNlOk4vQTpSMTdfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 06 Nov 2019 13:31:49
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : DELL
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R17_AMR.0
SUBROUTINE TF.MBL.I.BTB.CONSTRAIN
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :                              MODIFIED BY: Mahmudur Rahman
* Modified line 164 change field SCT.CONTRACT.AVAIL.AMT with SCT.CONTRACT.AMT
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_GTS.COMMON
    $INSERT I_F.BD.SCT.CAPTURE
    $INSERT I_F.BD.BTB.JOB.REGISTER
*-----------------------------------------------------------------------------
    $USING EB.SystemTables
    $USING EB.DataAccess
    $USING EB.ErrorProcessing
    $USING ST.CurrencyConfig
    $USING EB.OverrideProcessing
    IF EB.SystemTables.getVFunction() EQ 'D' THEN RETURN
    
    GOSUB INIT
    GOSUB OPENFILES
    GOSUB PROCESS
RETURN

INIT:
    FN.BD.BTB.JOB.REGISTER = 'F.BD.BTB.JOB.REGISTER'
    F.BD.BTB.JOB.REGISTER = ''
    FN.CCY = 'F.CURRENCY'
    F.CCY = ''
    Y.JOB.REG.ID = EB.SystemTables.getRNew(SCT.BTB.JOB.NO)
    Y.ERR.FLAG = '0'
    Y.OLD.CONTRACT.AMT = ''
    Y.NEW.CONTRACT.AMT = ''
    Y.CONTRACT.AMT = ''
    Y.CONTRACT.AVAIL.AMT = ''
    Y.CONTRACT.AMT = ''
    Y.SCT.CCY = ''
    Y.JOB.CCY = ''
    Y.NET.FOB = ''
    Y.EXCHANGE.RATE = ''
    Y.BTB.ENT.RATE = ''
    Y.NET.FOB = ''
    Y.BTB.ENT.AMT = ''

    Y.PCECC.ENT.RATE = ''
    Y.PCECC.ENT.AMT = ''
    Y.TOT.CONT.ENT.AMT = ''
    
    IF EB.SystemTables.getVFunction() NE 'R' AND EB.SystemTables.getVFunction() NE 'H' THEN
        EB.SystemTables.setRNew(SCT.NET.FOB.VALUE, DROUND(Y.NET.FOB,2))
        EB.SystemTables.setRNew(SCT.BTB.ENT.AMT, DROUND(Y.BTB.ENT.AMT,2))
        EB.SystemTables.setRNew(SCT.PCECC.ENT.AMT, DROUND(Y.PCECC.ENT.AMT,2))
        EB.SystemTables.setRNew(SCT.TOT.CONT.ENT.AMT, DROUND(Y.TOT.CONT.ENT.AMT,2))
    END
RETURN

OPENFILES:
    EB.DataAccess.Opf(FN.BD.BTB.JOB.REGISTER,F.BD.BTB.JOB.REGISTER)
    EB.DataAccess.Opf(FN.CCY,F.CCY)
RETURN

PROCESS:
**** Checking Reverse Entry for replacement of export lc & for collected TF
    IF EB.SystemTables.getVFunction() EQ 'R' THEN
        TOT.REP.ELC = DCOUNT(EB.SystemTables.getRNew(SCT.REP.ELC.NO),VM)
        TOT.COLL.TF = DCOUNT(EB.SystemTables.getRNew(SCT.COLL.TF.ID),VM)
        IF (TOT.REP.ELC GE '1' OR TOT.COLL.TF GE '1') THEN
            EB.SystemTables.setAf(SCT.REP.ELC.NO)
            EB.SystemTables.setAf(SCT.COLL.TF.ID)
            EB.SystemTables.setE('REVERSE NOT POSSIBLE REPLACEMENT OR COLLECTION IS OCCURED')
            EB.ErrorProcessing.StoreEndError()
            RETURN
        END
    END ELSE
        IF EB.SystemTables.getVFunction() NE 'H' THEN
            GOSUB VALUE.ASSIGN
            GOSUB FIELD.VALIDATION
        END
    END
RETURN

VALUE.ASSIGN:
*------ Contract Available Amount assigning
    Y.OLD.CONTRACT.AMT = EB.SystemTables.getROld(SCT.CONTRACT.AMT)
    Y.NEW.CONTRACT.AMT = EB.SystemTables.getRNew(SCT.CONTRACT.AMT)
    Y.CONTRACT.AMT.NAU = EB.SystemTables.getRNew(SCT.CONTRACT.AMT.NAU)
    Y.CONTRACT.CHG.AMT = Y.NEW.CONTRACT.AMT - Y.OLD.CONTRACT.AMT
    Y.CONTRACT.AVAIL.AMT = EB.SystemTables.getROld(SCT.CONTRACT.AVAIL.AMT)
    Y.CONTRACT.AVAIL.AMT =  Y.CONTRACT.CHG.AMT + Y.CONTRACT.AVAIL.AMT

    IF Y.NEW.CONTRACT.AMT LT Y.CONTRACT.AMT.NAU THEN
        EB.SystemTables.setAf(SCT.CONTRACT.AMT)
        EB.SystemTables.setEtext('Unauth Record Exist.Contract Amount cannot be less than ':Y.CONTRACT.AMT.NAU)
        EB.ErrorProcessing.StoreEndError()
        RETURN
    END ELSE
        EB.SystemTables.setRNew(SCT.CONTRACT.AVAIL.AMT, DROUND(Y.CONTRACT.AVAIL.AMT,2))
    END
    
***Find Exchange Rate
    Y.SCT.CCY = EB.SystemTables.getRNew(SCT.CURRENCY)
    IF Y.SCT.CCY EQ "" THEN
        EB.SystemTables.setAf(SCT.CURRENCY)
        EB.SystemTables.setEtext("Currency not set")
        EB.ErrorProcessing.StoreEndError()
    END
    Y.JOB.CCY = EB.SystemTables.getRNew(SCT.JOB.CURRENCY)
    IF Y.JOB.CCY EQ "" THEN
        EB.SystemTables.setAf(SCT.JOB.CURRENCY)
        EB.SystemTables.setEtext("Currency not set")
        EB.ErrorProcessing.StoreEndError()
    END
    EB.DataAccess.FRead(FN.CCY, Y.SCT.CCY, R.SCT.CCY.REC, F.CCY, Y.SCT.CCY.ERR)
    EB.DataAccess.FRead(FN.CCY, Y.JOB.CCY, R.JOB.CCY.REC, F.CCY, Y.JOB.CCY.ERR)
    
    
    BEGIN CASE
        CASE Y.SCT.CCY EQ Y.JOB.CCY
            EB.SystemTables.setRNew(SCT.JOB.EXCHG.RATE, '1')
            Y.EXCHANGE.RATE = '1'
        CASE Y.SCT.CCY NE Y.JOB.CCY AND Y.SCT.CCY EQ EB.SystemTables.getLccy()
            Y.CCY.MKT = '3'
            FIND Y.CCY.MKT IN R.JOB.CCY.REC<ST.CurrencyConfig.Currency.EbCurCurrencyMarket> SETTING Y.CCY.MKT.POS1, Y.CCY.MKT.POS2, Y.CCY.MKT.POS3 THEN
                Y.JOB.CCY.EXC.RATE = R.JOB.CCY.REC<ST.CurrencyConfig.Currency.EbCurSellRate, Y.CCY.MKT.POS2>
            END
            IF Y.JOB.CCY.EXC.RATE EQ "0" OR Y.JOB.CCY.EXC.RATE EQ "" THEN
                EB.SystemTables.setEtext("Currency Rate missing. Please update currency market")
                EB.ErrorProcessing.StoreEndError()
            END
            Y.EXCHANGE.RATE = DROUND((1 / Y.JOB.CCY.EXC.RATE),4)
            EB.SystemTables.setRNew(SCT.JOB.EXCHG.RATE, Y.EXCHANGE.RATE)
            
        CASE Y.SCT.CCY NE Y.JOB.CCY AND Y.SCT.CCY NE EB.SystemTables.getLccy()
            Y.CCY.MKT = '3'
            FIND Y.CCY.MKT IN R.SCT.CCY.REC<ST.CurrencyConfig.Currency.EbCurCurrencyMarket> SETTING Y.CCY.MKT.POS1, Y.CCY.MKT.POS2, Y.CCY.MKT.POS3 THEN
                Y.SCT.CCY.EXC.RATE = R.SCT.CCY.REC<ST.CurrencyConfig.Currency.EbCurSellRate, Y.CCY.MKT.POS2>
            END
            Y.CCY.MKT = '3'
            FIND Y.CCY.MKT IN R.JOB.CCY.REC<ST.CurrencyConfig.Currency.EbCurCurrencyMarket> SETTING Y.CCY.MKT.POS1, Y.CCY.MKT.POS2, Y.CCY.MKT.POS3 THEN
                Y.JOB.CCY.EXC.RATE = R.JOB.CCY.REC<ST.CurrencyConfig.Currency.EbCurSellRate, Y.CCY.MKT.POS2>
            END
            IF Y.SCT.CCY.EXC.RATE EQ "" OR Y.SCT.CCY.EXC.RATE EQ "0" OR Y.JOB.CCY.EXC.RATE EQ "0" OR Y.JOB.CCY.EXC.RATE EQ "" THEN
                EB.SystemTables.setEtext("Currency Rate missing. Please update currency market")
                EB.ErrorProcessing.StoreEndError()
            END
            Y.EXCHANGE.RATE = DROUND((Y.SCT.CCY.EXC.RATE / Y.JOB.CCY.EXC.RATE),4)
            
            EB.SystemTables.setRNew(SCT.JOB.EXCHG.RATE, Y.EXCHANGE.RATE)
    END CASE
 
*---- Assigning net fob value
    Y.NET.FOB = EB.SystemTables.getRNew(SCT.CONTRACT.AMT) - EB.SystemTables.getRNew(SCT.FREIGHT.CHARGES) - EB.SystemTables.getRNew(SCT.FOREIGN.CHARGES) - EB.SystemTables.getRNew(SCT.LOC.AGENT.COMM)
    EB.SystemTables.setRNew(SCT.NET.FOB.VALUE,DROUND(Y.NET.FOB,2))
    
*---- BTB, PCECC & total ent amount assigning
    Y.NET.FOB = EB.SystemTables.getRNew(SCT.NET.FOB.VALUE)
    Y.BTB.ENT.RATE = EB.SystemTables.getRNew(SCT.BTB.ENT.RATE)
*****************************erian@fortress-global.com******20201013************
    Y.BTB.ENT.AMT = (Y.NET.FOB * Y.BTB.ENT.RATE * Y.EXCHANGE.RATE)/100
*Y.BTB.ENT.AMT = (Y.NET.FOB * Y.BTB.ENT.RATE)/100
***********end******************erian@fortress-global.com******20201013************

    Y.PCECC.ENT.RATE = EB.SystemTables.getRNew(SCT.PCECC.ENT.RATE)
*****************************erian@fortress-global.com******20201013************
    Y.PCECC.ENT.AMT = (Y.NET.FOB * Y.PCECC.ENT.RATE * Y.EXCHANGE.RATE)/100
*Y.PCECC.ENT.AMT = (Y.NET.FOB * Y.PCECC.ENT.RATE)/100
************end***** ************erian@fortress-global.com******20201013************
    
    EB.SystemTables.setRNew(SCT.BTB.ENT.RATE, DROUND(Y.BTB.ENT.RATE,4))
    EB.SystemTables.setRNew(SCT.PCECC.ENT.RATE, DROUND(Y.PCECC.ENT.RATE,4))
    EB.SystemTables.setRNew(SCT.BTB.ENT.AMT, DROUND(Y.BTB.ENT.AMT,2))
    EB.SystemTables.setRNew(SCT.PCECC.ENT.AMT, DROUND(Y.PCECC.ENT.AMT,2))
    Y.TOT.CONT.ENT.AMT = EB.SystemTables.getRNew(SCT.BTB.ENT.AMT) + EB.SystemTables.getRNew(SCT.PCECC.ENT.AMT)
    EB.SystemTables.setRNew(SCT.TOT.CONT.ENT.AMT, DROUND(Y.TOT.CONT.ENT.AMT,2))
   
    IF Y.NEW.CONTRACT.AMT LT Y.OLD.CONTRACT.AMT THEN
        GOSUB CHECK.AVAILABLE.ENT.AMT
    END

RETURN

FIELD.VALIDATION:
    Y.CONTRACT.AVAIL.AMT = EB.SystemTables.getRNew(SCT.CONTRACT.AVAIL.AMT)
    Y.TOT.REP.OCCURED = DCOUNT(EB.SystemTables.getRNew(SCT.REP.ELC.NO),VM)
    Y.REPLACE.ALLOW = EB.SystemTables.getRNew(SCT.REPLACE.ALLOW.YN)
    Y.FULLY.REPLACE.YN = EB.SystemTables.getRNew(SCT.FULLY.REPLACE.YN)
    Y.FULLY.UTILIZED.YN = EB.SystemTables.getRNew(SCT.FULLY.UTILIZED.YN)
    Y.COLL.AWAIT.AMT = EB.SystemTables.getRNew(SCT.COLL.AWAIT.AMT)
    Y.COLL.TF.ID.CNT = DCOUNT(EB.SystemTables.getRNew(SCT.COLL.TF.ID),VM)
    Y.TOT.ENT.RATE = EB.SystemTables.getRNew(SCT.BTB.ENT.RATE) + EB.SystemTables.getRNew(SCT.PCECC.ENT.RATE)
 
    IF Y.TOT.REP.OCCURED GE '1' AND (Y.REPLACE.ALLOW EQ 'NO' OR Y.REPLACE.ALLOW EQ '') THEN
        EB.SystemTables.setAf(SCT.REPLACE.ALLOW.YN)
        EB.SystemTables.setEtext('ELC REPLACEMENT OCCURE.REPLACEMENT ALLOW SHOULD BE YES')
        EB.ErrorProcessing.StoreEndError()
        RETURN
    END

    IF (Y.CONTRACT.AVAIL.AMT EQ '0' OR Y.CONTRACT.AVAIL.AMT EQ '') AND Y.REPLACE.ALLOW EQ 'YES' THEN
        IF (Y.FULLY.REPLACE.YN EQ 'NO' OR Y.FULLY.REPLACE.YN EQ '') THEN
            EB.SystemTables.setAf(SCT.FULLY.REPLACE.YN)
            EB.SystemTables.setEtext('NO AVAILABLE AMOUNT. FULLY REPLACE SHOULD BE YES')
            EB.ErrorProcessing.StoreEndError()
            RETURN
        END
    END

    IF (Y.CONTRACT.AVAIL.AMT EQ '0' OR Y.CONTRACT.AVAIL.AMT EQ '') AND (Y.REPLACE.ALLOW EQ 'NO' OR Y.REPLACE.ALLOW EQ '') THEN
        IF (Y.FULLY.REPLACE.YN EQ 'NO' OR Y.FULLY.REPLACE.YN EQ 'YES') THEN
            EB.SystemTables.setAf(SCT.FULLY.REPLACE.YN)
            EB.SystemTables.setEtext('NO AVAILABLE AMOUNT. FULLY REPLACE SHOULD BE NONE')
            EB.ErrorProcessing.StoreEndError()
            RETURN
        END
    END
    
    IF Y.CONTRACT.AVAIL.AMT GT '0' AND Y.REPLACE.ALLOW EQ 'YES' THEN
        IF Y.FULLY.REPLACE.YN EQ '' THEN
            EB.SystemTables.setAf(SCT.FULLY.REPLACE.YN)
            EB.SystemTables.setEtext('REPLACE ALLOW SELECTED AS "YES". FULLY REPLACE SHOULD BE YES OR NO')
            EB.ErrorProcessing.StoreEndError()
            RETURN
        END
    END
    
    IF Y.CONTRACT.AVAIL.AMT GT '0' AND (Y.REPLACE.ALLOW EQ 'NO' OR Y.REPLACE.ALLOW EQ '') THEN
        IF Y.FULLY.REPLACE.YN NE '' THEN
            EB.SystemTables.setAf(SCT.FULLY.REPLACE.YN)
            EB.SystemTables.setEtext('REPLACEMENT ALLOW SELECTED AS NO OR NONE. FULLY REPLACE SHOULD BE NONE')
            EB.ErrorProcessing.StoreEndError()
            RETURN
        END
    END
    
    IF Y.COLL.AWAIT.AMT GT '0' OR Y.COLL.TF.ID.CNT GT '0' THEN
        IF Y.FULLY.UTILIZED.YN EQ '' THEN
            EB.SystemTables.setAf(SCT.FULLY.UTILIZED.YN)
            EB.SystemTables.setEtext('FULLY UTILIZED SHOULD NOT BE NONE')
            EB.ErrorProcessing.StoreEndError()
            RETURN
        END
    END
    
    IF (Y.COLL.AWAIT.AMT EQ '0' OR Y.COLL.AWAIT.AMT EQ '' OR Y.COLL.TF.ID.CNT EQ '0' OR Y.COLL.TF.ID.CNT EQ '') THEN
        IF Y.FULLY.UTILIZED.YN NE '' THEN
            EB.SystemTables.setAf(SCT.FULLY.UTILIZED.YN)
            EB.SystemTables.setEtext('FULLY UTILIZED SHOULD BE NONE')
            EB.ErrorProcessing.StoreEndError()
            RETURN
        END
    END
    
    IF Y.TOT.ENT.RATE GT '100' THEN
        EB.SystemTables.setAf(SCT.BTB.ENT.RATE)
        EB.SystemTables.setEtext('Total Rate of Entitlement Cannot Exceed 100')
        EB.ErrorProcessing.StoreEndError()
        RETURN
    END
    
** Checking repalce allow or not
*    Y.TOT.REP.OCCURED = DCOUNT(EB.SystemTables.getRNew(SCT.REP.ELC.NO),FM)
*    Y.REPLACE.ALLOW = EB.SystemTables.getRNew(SCT.REPLACE.ALLOW.YN)
*
*    IF (Y.REPLACE.ALLOW EQ 'NO' AND  Y.TOT.REP.OCCURED GE '1') THEN
*        EB.SystemTables.setAf(SCT.REPLACE.ALLOW.YN)
*        EB.SystemTables.setEtext('Cannot Change Statue Marking.Already ELC Replacement Occured')
*        EB.ErrorProcessing.StoreEndError()
*        RETURN
*    END
*
**Checking fully replace and utilized allowed or not
*    Y.CONTRACT.AVAIL.AMT = EB.SystemTables.getRNew(SCT.CONTRACT.AVAIL.AMT)
*    Y.FULLY.REPLACE.YN = EB.SystemTables.getRNew(SCT.FULLY.REPLACE.YN)
*    Y.FULLY.UTILIZED.YN = EB.SystemTables.getRNew(SCT.FULLY.UTILIZED.YN)
*    IF (Y.CONTRACT.AVAIL.AMT EQ '0' OR Y.CONTRACT.AVAIL.AMT EQ '') THEN
*        IF Y.FULLY.REPLACE.YN EQ 'NO' THEN
*            EB.SystemTables.setAf(SCT.FULLY.REPLACE.YN)
*            EB.SystemTables.setEtext('FULLY REPLACE MUST BE YES')
*            EB.ErrorProcessing.StoreEndError()
*            RETURN
*        END
*        IF Y.FULLY.UTILIZED.YN EQ 'NO' THEN
*            EB.SystemTables.setAf(SCT.FULLY.UTILIZED.YN)
*            EB.SystemTables.setEtext('FULLY UTILIZED MUST BE YES')
*            EB.ErrorProcessing.StoreEndError()
*            RETURN
*        END
*    END
*
*    GOSUB REPLACE.ALLOW.YN.CHECK
*
*    Y.BTB.ENT.RATE = EB.SystemTables.getRNew(SCT.BTB.ENT.RATE)
*    Y.PCECC.ENT.RATE = EB.SystemTables.getRNew(SCT.PCECC.ENT.RATE)
*    Y.TOT.ERATE = Y.BTB.ENT.RATE + Y.PCECC.ENT.RATE
*    IF Y.TOT.ERATE GT '100' THEN
*        EB.SystemTables.setEtext('Total Rate of Entitlement Cannot Exceed 100')
*        EB.SystemTables.setAf(SCT.BTB.ENT.RATE)
*        EB.SystemTables.setAf(SCT.PCECC.ENT.RATE)
*        EB.ErrorProcessing.StoreEndError()
*        RETURN
*    END
*
*RETURN
*
*REPLACE.ALLOW.YN.CHECK:
*
*    Y.SCT.REPLACE.ALLOW.YN = EB.SystemTables.getRNew(SCT.REPLACE.ALLOW.YN)
*    Y.SCT.FULLY.REPLACE.YN = EB.SystemTables.getRNew(SCT.FULLY.REPLACE.YN)
*    Y.SCT.FULLY.UTILIZED.YN = EB.SystemTables.getRNew(SCT.FULLY.UTILIZED.YN)
*
*    IF Y.SCT.REPLACE.ALLOW.YN NE '' THEN
*        IF Y.SCT.FULLY.UTILIZED.YN NE '' THEN
*            EB.SystemTables.setAf(SCT.FULLY.UTILIZED.YN)
*            EB.SystemTables.setEtext('FULLY UTILIZED MUST BE NONE')
*            EB.ErrorProcessing.StoreEndError()
*            RETURN
*        END
*        IF Y.SCT.REPLACE.ALLOW.YN EQ 'YES'  THEN
*            IF Y.SCT.FULLY.REPLACE.YN EQ 'YES' THEN
*                EB.SystemTables.setAf(SCT.FULLY.REPLACE.YN)
*                EB.SystemTables.setEtext('FULLY REPLACE EITHER NONE OR NO')
*                EB.ErrorProcessing.StoreEndError()
*                RETURN
*            END
*        END
*        ELSE
*            IF Y.SCT.FULLY.REPLACE.YN NE '' THEN
*                EB.SystemTables.setAf(SCT.FULLY.REPLACE.YN)
*                EB.SystemTables.setEtext('FULLY REPLACE MUST BE NONE')
*                EB.ErrorProcessing.StoreEndError()
*                RETURN
*            END
*        END
*    END
*    ELSE
*        IF Y.SCT.FULLY.UTILIZED.YN EQ '' THEN
*            EB.SystemTables.setAf(SCT.REPLACE.ALLOW.YN)
*            EB.SystemTables.setAf(SCT.FULLY.UTILIZED.YN)
*            EB.SystemTables.setEtext('INPUT EITHER REPLACE ALLOW OR FULLY.UTILIZED')
*            EB.ErrorProcessing.StoreEndError()
*            RETURN
*        END
*        IF Y.SCT.FULLY.REPLACE.YN NE '' THEN
*            EB.SystemTables.setAf(SCT.FULLY.REPLACE.YN)
*            EB.SystemTables.setEtext('FULLY REPLACE MUST BE NONE')
*            EB.ErrorProcessing.StoreEndError()
*            RETURN
*        END
*    END

RETURN

CHECK.AVAILABLE.ENT.AMT:
*To check the new entitle amount exceed the job entitle amount
    IF OFS$BROWSER AND  OFS$OPERATION EQ 'PROCESS' THEN
        EB.DataAccess.FRead(FN.BD.BTB.JOB.REGISTER, Y.JOB.REG.ID, REC.JOB.REG, F.BD.BTB.JOB.REGISTER, ERR.JOB.REG)
        Y.TOT.BTB.AVL.AMT = REC.JOB.REG<BTB.JOB.TOT.BTB.AVL.AMT>
        Y.BTB.ENT.AMT.OLD = EB.SystemTables.getROld(SCT.BTB.ENT.AMT)
        Y.CONV.BTB.ENT.AMT = ABS((Y.BTB.ENT.AMT - Y.BTB.ENT.AMT.OLD) * Y.EXCHANGE.RATE) ;*to find BTB Entitlement changes Amount with Exchange rate

        IF Y.TOT.BTB.AVL.AMT LT Y.CONV.BTB.ENT.AMT  THEN ;* Check Reduced BTB Entitlement Amount is Available in BTB JOB Register
            Y.OVERRIDE.VAL = EB.SystemTables.getRNew(EB.SystemTables.getV()-9)
            Y.OVRRD.NO = DCOUNT(Y.OVERRIDE.VAL,VM) + 1
            EB.SystemTables.setText('Available BTB JOB Entitlement is less than Decreased Contract BTB Entitlement')
            EB.OverrideProcessing.StoreOverride(Y.OVRRD.NO)
        END

        Y.TOT.PC.AVL.AMT = REC.JOB.REG<BTB.JOB.TOT.PC.AVL.AMT>
        Y.PCECC.ENT.AMT.OLD = EB.SystemTables.getROld(SCT.PCECC.ENT.AMT)
        Y.CONV.PCECC.ENT.AMT = ABS((Y.PCECC.ENT.AMT - Y.PCECC.ENT.AMT.OLD) * Y.EXCHANGE.RATE) ;*to find PC/ECC Entitlement changes Amount with Exchange rate

        IF Y.TOT.PC.AVL.AMT LT Y.CONV.PCECC.ENT.AMT THEN ;* Check Reduced PC/ECC Entitlement Amount is Available in BTB JOB Register
            Y.OVERRIDE.VAL = EB.SystemTables.getRNew(EB.SystemTables.getV()-9)
            Y.OVRRD.NO = DCOUNT(Y.OVERRIDE.VAL,VM) + 1
            EB.SystemTables.setText('Available PC/ECC JOB Entitlement is less than Decreased Contract PC/ECC Entitlement')
            EB.OverrideProcessing.StoreOverride(Y.OVRRD.NO)
        END
    END
    
RETURN


END
