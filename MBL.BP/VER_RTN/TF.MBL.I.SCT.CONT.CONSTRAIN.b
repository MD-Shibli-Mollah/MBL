* @ValidationCode : Mjo3NjAxMzIxMTE6Q3AxMjUyOjE1NzM0NzM1NzMyNzg6REVMTDotMTotMTowOjA6ZmFsc2U6Ti9BOlIxN19BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 11 Nov 2019 17:59:33
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : DELL
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R17_AMR.0
SUBROUTINE TF.MBL.I.SCT.CONT.CONSTRAIN
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.BD.SCT.CAPTURE
    $INSERT I_F.BD.LC.AD.CODE
    $INSERT I_F.BD.BTB.JOB.REGISTER
*-----------------------------------------------------------------------------
    $USING EB.SystemTables
    $USING EB.ErrorProcessing
    $USING EB.OverrideProcessing
    $USING ST.CompanyCreation
    $USING EB.Foundation
    $USING EB.DataAccess
    $USING ST.CurrencyConfig
    
    GOSUB INIT
    GOSUB OPENFILES
    GOSUB PROCESS
RETURN

INIT:
    FN.COMPANY = 'F.COMPANY';   F.COMPANY = ''
    FN.CCY = 'F.CURRENCY';      F.CCY = ''
    FN.BD.LC.AD.CODE= 'F.BD.LC.AD.CODE';     F.BD.LC.AD.CODE = ''
    Y.EXCHANGE.RATE = ''
    Y.COMPANY.ID = EB.SystemTables.getIdCompany()
    Y.ERR.FLAG = '0'
    FN.BTB.JOB.REGISTER = 'F.BD.BTB.JOB.REGISTER'
    F.BTB.JOB.REGISTER = ''
RETURN

OPENFILES:
    EB.DataAccess.Opf(FN.COMPANY,F.COMPANY)
    EB.DataAccess.Opf(FN.CCY,F.CCY)
    EB.DataAccess.Opf(FN.BD.LC.AD.CODE,F.BD.LC.AD.CODE)
    EB.DataAccess.Opf(FN.BTB.JOB.REGISTER, F.BTB.JOB.REGISTER)
RETURN

PROCESS:
    GOSUB VALUE.ASSIGN
    GOSUB FIELD.VALIDATION
    IF Y.ERR.FLAG EQ '0' THEN
        GOSUB OVERRIDE.GENERATE
    END
RETURN

VALUE.ASSIGN:
*------ Contract Available Amount assigning
    Y.CONTRACT.AMT = EB.SystemTables.getRNew(SCT.CONTRACT.AMT)
    Y.CON.USE.AMT = EB.SystemTables.setRNew(SCT.CONTRACT.USE.AMT,"0")
    Y.CON.AMT.NAU = EB.SystemTables.setRNew(SCT.CONTRACT.AMT.NAU,"0")
    Y.CON.AVAIL.AMT = EB.SystemTables.setRNew(SCT.CONTRACT.AVAIL.AMT, Y.CONTRACT.AMT)

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
            Y.CCY.MKT = '1'
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
            Y.CCY.MKT = '1'
            FIND Y.CCY.MKT IN R.SCT.CCY.REC<ST.CurrencyConfig.Currency.EbCurCurrencyMarket> SETTING Y.CCY.MKT.POS1, Y.CCY.MKT.POS2, Y.CCY.MKT.POS3 THEN
                Y.SCT.CCY.EXC.RATE = R.SCT.CCY.REC<ST.CurrencyConfig.Currency.EbCurSellRate, Y.CCY.MKT.POS2>
            END
            Y.CCY.MKT = '1'
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
    
    Y.NET.FOB = EB.SystemTables.getRNew(SCT.CONTRACT.AVAIL.AMT) - EB.SystemTables.getRNew(SCT.FREIGHT.CHARGES) - EB.SystemTables.getRNew(SCT.FOREIGN.CHARGES) - EB.SystemTables.getRNew(SCT.LOC.AGENT.COMM)
    EB.SystemTables.setRNew(SCT.NET.FOB.VALUE, DROUND(Y.NET.FOB,2))

*    ---- BTB, PCECC & total ent amount assigning
    Y.BTB.ENT.RATE = EB.SystemTables.getRNew(SCT.BTB.ENT.RATE)
    Y.NET.FOB = EB.SystemTables.getRNew(SCT.NET.FOB.VALUE)
*****************************erian@fortress-global.com******20201013************
    Y.BTB.ENT.AMT = (Y.NET.FOB * Y.BTB.ENT.RATE * Y.EXCHANGE.RATE)/100
*Y.BTB.ENT.AMT = (Y.NET.FOB * Y.BTB.ENT.RATE)/100
***********end******************erian@fortress-global.com******20201013************
    EB.SystemTables.setRNew(SCT.BTB.ENT.AMT, DROUND(Y.BTB.ENT.AMT,2))

    Y.PCECC.ENT.RATE = EB.SystemTables.getRNew(SCT.PCECC.ENT.RATE)
*****************************erian@fortress-global.com******20201013************
    Y.PCECC.ENT.AMT = (Y.NET.FOB * Y.PCECC.ENT.RATE * Y.EXCHANGE.RATE)/100
*Y.PCECC.ENT.AMT = (Y.NET.FOB * Y.PCECC.ENT.RATE)/100
************end*****************erian@fortress-global.com******20201013************
    EB.SystemTables.setRNew(SCT.PCECC.ENT.AMT, DROUND(Y.PCECC.ENT.AMT,2))

    Y.TOT.CONT.ENT.AMT = EB.SystemTables.getRNew(SCT.BTB.ENT.AMT) + EB.SystemTables.getRNew(SCT.PCECC.ENT.AMT)
    EB.SystemTables.setRNew(SCT.TOT.CONT.ENT.AMT, DROUND(Y.TOT.CONT.ENT.AMT,2))
RETURN

FIELD.VALIDATION:

    Y.TODAY = EB.SystemTables.getToday()

    IF EB.SystemTables.getRNew(SCT.CONTRACT.AVAIL.AMT) EQ '' OR EB.SystemTables.getRNew(SCT.CONTRACT.AVAIL.AMT) EQ '0' THEN
        EB.SystemTables.setAf(SCT.CONTRACT.AMT)
        EB.SystemTables.setEtext('MISSING SCT CONTRACT AMT')
        EB.ErrorProcessing.StoreEndError()
        Y.ERR.FLAG = '1'
        RETURN
    END
    IF EB.SystemTables.getRNew(SCT.CURRENCY) EQ '' THEN
        EB.SystemTables.setAf(SCT.CURRENCY)
        EB.SystemTables.setEtext('MISSING SCT CURRENCY')
        EB.ErrorProcessing.StoreEndError()
        Y.ERR.FLAG = '1'
        RETURN
    END
    
*IF EB.SystemTables.getRNew(SCT.JOB.CURRENCY) EQ '' THEN
*EB.SystemTables.setAf(SCT.JOB.CURRENCY)
*EB.SystemTables.setEtext('JOB CURRENCY MISSING')
*EB.ErrorProcessing.StoreEndError()
*Y.ERR.FLAG = '1'
*RETURN
*END

    Y.TOT.PERCENT = EB.SystemTables.getComi() + EB.SystemTables.getRNew(SCT.PCECC.ENT.RATE)
    IF Y.TOT.PERCENT GT '100' THEN
        EB.SystemTables.setAf(SCT.BTB.ENT.RATE)
        EB.SystemTables.setAf(SCT.PCECC.ENT.RATE)
        EB.SystemTables.setEtext('EXCEED 100 PERCENT')
        EB.ErrorProcessing.StoreEndError()
        Y.ERR.FLAG = '1'
        RETURN
    END
    IF EB.SystemTables.getRNew(SCT.EXPIRY.DATE) LT Y.TODAY THEN
        EB.SystemTables.setAf(SCT.EXPIRY.DATE)
        EB.SystemTables.setEtext('EXPIRY DATE LESS TODAY')
        EB.ErrorProcessing.StoreEndError()
        Y.ERR.FLAG = '1'
        RETURN
    END
    IF EB.SystemTables.getRNew(SCT.TOT.CONT.ENT.AMT) EQ '' OR EB.SystemTables.getRNew(SCT.TOT.CONT.ENT.AMT) EQ '0' THEN
        EB.SystemTables.setAf(SCT.TOT.CONT.ENT.AMT)
        EB.SystemTables.setEtext('SCT.TOT.CONT.ENT.AMT CAN NOT BE 0')
        EB.ErrorProcessing.StoreEndError()
        Y.ERR.FLAG = '1'
        RETURN
    END

    IF EB.SystemTables.getRNew(SCT.NEW.EXIST.JOB.NO) EQ '' THEN
        EB.SystemTables.setAf(SCT.NEW.EXIST.JOB.NO)
        EB.SystemTables.setEtext("SCT.NEW.EXIST.JOB.NO CAN NOT BE ''")
        EB.ErrorProcessing.StoreEndError()
        Y.ERR.FLAG = '1'
        RETURN
    END
****************************
    IF EB.SystemTables.getRNew(SCT.NEW.EXIST.JOB.NO) EQ 'EXIST' THEN
        EB.DataAccess.FRead(FN.BTB.JOB.REGISTER, EB.SystemTables.getRNew(SCT.BTB.JOB.NO), R.BTB.JOB.REG, F.BTB.JOB.REGISTER, ERR.BTB.JOB.REGISTER)
        IF R.BTB.JOB.REG THEN
            Y.JOB.JOB.CURR = R.BTB.JOB.REG<BTB.JOB.JOB.CURRENCY>
            Y.LC.JOB.CURR = EB.SystemTables.getRNew(SCT.JOB.CURRENCY)
            IF Y.JOB.JOB.CURR NE Y.LC.JOB.CURR THEN
                EB.SystemTables.setAf(SCT.JOB.CURRENCY)
                EB.SystemTables.setEtext("Job Currency Differ with existing")
                EB.ErrorProcessing.StoreEndError()
                Y.ERR.FLAG = '1'
                RETURN
            END
        END
*        EB.SystemTables.setAf(SCT.NEW.EXIST.JOB.NO)
*        EB.SystemTables.setEtext("SCT.NEW.EXIST.JOB.NO CAN NOT BE ''")
*        EB.ErrorProcessing.StoreEndError()
*        Y.ERR.FLAG = '1'
*        RETURN
    END
****************************
    IF EB.SystemTables.getRNew(SCT.CONTRACT.AMT) LE '0' THEN
        EB.SystemTables.setAf(SCT.CONTRACT.AMT)
        EB.SystemTables.setEtext("CONTRACT AMOUNT MUST BE GT 0")
        EB.ErrorProcessing.StoreEndError()
        Y.ERR.FLAG = '1'
        RETURN
    END

    Y.NET.FOB = EB.SystemTables.getRNew(SCT.NET.FOB.VALUE)
    Y.BTB.ENT.RATE = EB.SystemTables.getRNew(SCT.BTB.ENT.RATE)
    Y.PCECC.ENT.RATE = EB.SystemTables.getRNew(SCT.PCECC.ENT.RATE)
    Y.TOT.ERATE = Y.BTB.ENT.RATE + Y.PCECC.ENT.RATE
    IF Y.TOT.ERATE GT '100' THEN
        EB.SystemTables.setEtext('Total Rate of Entitlement Cannot Exceed 100')
        EB.SystemTables.setAf(SCT.BTB.ENT.RATE)
        EB.SystemTables.setAf(SCT.PCECC.ENT.RATE)
        EB.ErrorProcessing.StoreEndError()
        RETURN
    END

    EB.DataAccess.FRead(FN.BD.LC.AD.CODE,EB.SystemTables.getIdCompany(),R.BD.LC.AD.CODE,F.BD.LC.AD.CODE,BD.LC.AD.CODE.ERR)
    IF BD.LC.AD.CODE.ERR THEN
        EB.SystemTables.setE("AD CODE DOES NOT EXIST FOR THIS COMPANY")
        EB.ErrorProcessing.StoreEndError()
        Y.ERR.FLAG = '1'
        RETURN
    END ELSE
        Y.AD.BR.CODE = R.BD.LC.AD.CODE<AD.CODE.AD.CODE>
    END
    GOSUB REPLACE.ALLOW.YN.CHECK
    
RETURN

REPLACE.ALLOW.YN.CHECK:

    Y.SCT.REPLACE.ALLOW.YN = EB.SystemTables.getRNew(SCT.REPLACE.ALLOW.YN)
    Y.SCT.FULLY.REPLACE.YN = EB.SystemTables.getRNew(SCT.FULLY.REPLACE.YN)
    Y.SCT.FULLY.UTILIZED.YN = EB.SystemTables.getRNew(SCT.FULLY.UTILIZED.YN)
    
    IF Y.SCT.REPLACE.ALLOW.YN NE '' THEN
*        IF Y.SCT.FULLY.UTILIZED.YN NE '' THEN
*            EB.SystemTables.setAf(SCT.FULLY.UTILIZED.YN)
*            EB.SystemTables.setEtext('FULLY UTILIZED MUST BE NONE')
*            EB.ErrorProcessing.StoreEndError()
*            Y.ERR.FLAG = '1'
*            RETURN
*        END
        IF Y.SCT.REPLACE.ALLOW.YN EQ 'YES'  THEN
            EB.SystemTables.setRNew(SCT.FULLY.REPLACE.YN, 'NO')
*            IF Y.SCT.FULLY.REPLACE.YN EQ 'YES' THEN
*                EB.SystemTables.setAf(SCT.FULLY.REPLACE.YN)
*                EB.SystemTables.setEtext('FULLY REPLACE EITHER NONE OR NO')
*                EB.ErrorProcessing.StoreEndError()
*                Y.ERR.FLAG = '1'
*                RETURN
*            END
        END ELSE
            IF Y.SCT.FULLY.REPLACE.YN NE '' THEN
                EB.SystemTables.setAf(SCT.REPLACE.ALLOW.YN)
                EB.SystemTables.setEtext('FULLY REPLACE MUST BE NONE')
                EB.ErrorProcessing.StoreEndError()
                Y.ERR.FLAG = '1'
                RETURN
            END
        END
    END ELSE
        EB.SystemTables.setAf(SCT.REPLACE.ALLOW.YN)
        EB.SystemTables.setEtext('REPLACE ALLOW IS MANDATORY')
        EB.ErrorProcessing.StoreEndError()
        Y.ERR.FLAG = '1'
        RETURN
*        IF Y.SCT.FULLY.UTILIZED.YN EQ '' THEN
*            EB.SystemTables.setAf(SCT.FULLY.UTILIZED.YN)
*            EB.SystemTables.setAf(SCT.REPLACE.ALLOW.YN)
*            EB.SystemTables.setEtext('INPUT EITHER REPLACE ALLOW OR FULLY.UTILIZED')
*            EB.ErrorProcessing.StoreEndError()
*            Y.ERR.FLAG = '1'
*            RETURN
*        END
*        IF Y.SCT.FULLY.REPLACE.YN NE '' THEN
*            EB.SystemTables.setAf(SCT.FULLY.REPLACE.YN)
*            EB.SystemTables.setEtext('FULLY REPLACE MUST BE NONE')
*            EB.ErrorProcessing.StoreEndError()
*            Y.ERR.FLAG = '1'
*            RETURN
*        END
    END

RETURN

OVERRIDE.GENERATE:
    Y.SHIPMENT.DT = EB.SystemTables.getRNew(SCT.SHIPMENT.DATE)
    IF Y.SHIPMENT.DT NE '' THEN
        IF EB.SystemTables.getRNew(SCT.SHIPMENT.DATE) LT Y.TODAY THEN
            Y.OVERIDE.MESSAGE = 'SHIPMENT DATE LESS THAN TODAY'
            Y.OVERRIDE.MESSAGE.ALL = EB.SystemTables.getRNew(SCT.OVERRIDE)
            FINDSTR Y.OVERIDE.MESSAGE IN Y.OVERRIDE.MESSAGE.ALL SETTING MESSAGE.POS1, MESSAGE.POS2 ELSE NULL
            IF MESSAGE.POS1 EQ '' THEN
                Y.OVERRIDE.VAL = EB.SystemTables.getRNew(EB.SystemTables.getV()-9)
                Y.OVRRD.NO = DCOUNT(Y.OVERRIDE.VAL,VM) + 1
                EB.SystemTables.setText('SHIPMENT DATE LESS THAN TODAY')
                EB.OverrideProcessing.StoreOverride(Y.OVRRD.NO)
            END
        END
    END
    
    Y.CONT.DT = EB.SystemTables.getRNew(SCT.CONTRACT.DATE)
    IF Y.CONT.DT NE '' THEN
        IF EB.SystemTables.getRNew(SCT.CONTRACT.DATE) LT Y.TODAY THEN
            Y.OVERIDE.MESSAGE = 'CONTRACT DATE LESS THAN TODAY'
            Y.OVERRIDE.MESSAGE.ALL = EB.SystemTables.getRNew(SCT.OVERRIDE)
            FINDSTR Y.OVERIDE.MESSAGE IN Y.OVERRIDE.MESSAGE.ALL SETTING MESSAGE.POS1, MESSAGE.POS2 ELSE NULL
            IF MESSAGE.POS1 EQ '' THEN
                Y.OVERRIDE.VAL = EB.SystemTables.getRNew(EB.SystemTables.getV()-9)
                Y.OVRRD.NO = DCOUNT(Y.OVERRIDE.VAL,VM) + 1
                EB.SystemTables.setText('CONTRACT DATE LESS THAN TODAY')
                EB.OverrideProcessing.StoreOverride(Y.OVRRD.NO)
            END
        END
    END
RETURN
END
