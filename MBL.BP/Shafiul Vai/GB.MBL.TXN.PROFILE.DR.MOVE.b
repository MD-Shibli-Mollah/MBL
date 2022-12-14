* @ValidationCode : MjoyMDg1ODk0MzY2OkNwMTI1MjoxNTk1NDgyMTM0MjE5OkRFTEw6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 23 Jul 2020 11:28:54
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : DELL
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0
SUBROUTINE GB.MBL.TXN.PROFILE.DR.MOVE
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Developed by : s.azam@fortress-global.com
* Modification History :
* Transaction Profile Debit Override and Transaction Validation
*-----------------------------------------------------------------------------
   
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_AA.LOCAL.COMMON
    $INSERT I_AA.APP.COMMON
    $INSERT I_EB.EXTERNAL.COMMON
    $INSERT I_F.FT.TXN.TYPE.CONDITION
    $INSERT I_F.MBL.TXN.PROFILE
    $INSERT I_F.MBL.TXN.PROFILE.ENTRY
    $INSERT I_F.MBL.TXN.PROFILE.PARAM
        
    $USING EB.SystemTables
    $USING AA.Framework
    $USING FT.Contract
    $USING TT.Contract
    $USING TT.Config
    $USING TT.ModelBank
    $USING EB.DataAccess
    $USING EB.ErrorProcessing
    $USING EB.OverrideProcessing
        
*-----------------------------------------------------------------------------
    Y.AC.NO = c_aalocLinkedAccount
    Y.TXN.AMT =  c_aalocArrActivityRec<AA.Framework.ArrangementActivity.ArrActTxnAmount>
    Y.TXN.REF = FIELD(c_aalocArrActivityRec<AA.Framework.ArrangementActivity.ArrActTxnContractId>,'\',1)
    Y.SYS.ID = c_aalocArrActivityRec<AA.Framework.ArrangementActivity.ArrActTxnSystemId>
    Y.RECORD.STATUS = c_aalocActivityStatus
    
        
    IF Y.AC.NO MATCHES '3A...' THEN
        RETURN
    END
    
    GOSUB INIT
    GOSUB OPENFILES
    GOSUB PROCESS
RETURN


*****
INIT:
*****
    FN.TP = 'F.MBL.TXN.PROFILE'
    F.TP = ''
    FN.TP.ENTRY = 'F.MBL.TXN.PROFILE.ENTRY'
    F.TP.ENTRY = ''
    FN.PARAM = 'F.MBL.TXN.PROFILE.PARAM'
    F.PARAM = ''
    FN.TT = 'F.TELLER'
    F.TT = ''
    FN.TT.NAU = 'F.TELLER$NAU'
    F.TT.NAU = ''
    FN.FT = 'F.FUNDS.TRANSFER'
    F.FT = ''
    FN.FT.NAU = 'F.FUNDS.TRANSFER$NAU'
    F.FT.NAU = ''
    FN.FT.TYPE = 'F.FT.TXN.TYPE.CONDITION'
    F.FT.TYPE = ''
    FN.OR = 'F.OVERRIDE'
    F.OR = ''
    Y.TOT.TXN.AMT = ''
*    Y.TOT.WITH.TXN.CODE = '2':VM:'26':VM:'90':VM:'107':VM:'866':VM:'867':FM:'213':VM:'42':VM:'27'
*    Y.WITHDRAW.PARTICULAR ='Cash With. (Inc of Online & ATM)':FM:'Transfer/Payment by Instruments':FM:'Foreign Outward Remittance':FM:'Settlement of Import Payment':FM:'Deposit/Transfer to BO Account':FM:'Other Withdrawal'
RETURN

**********
OPENFILES:
**********

    EB.DataAccess.Opf(FN.TP,F.TP)
    EB.DataAccess.Opf(FN.TP.ENTRY,F.TP.ENTRY)
    EB.DataAccess.Opf(FN.PARAM,F.PARAM)
    EB.DataAccess.Opf(FN.FT,F.FT)
    EB.DataAccess.Opf(FN.TT,F.TT)
    EB.DataAccess.Opf(FN.FT.NAU,F.FT.NAU)
    EB.DataAccess.Opf(FN.TT.NAU,F.TT.NAU)
    EB.DataAccess.Opf(FN.FT.TYPE,F.FT.TYPE)
    EB.DataAccess.Opf(FN.OR,F.OR)

RETURN

********
PROCESS:
********
    Y.PARAM.ID = 'SYSTEM'
    EB.DataAccess.FRead(FN.PARAM,Y.PARAM.ID,R.PARAM, F.PARAM, ER.PARAM)
    Y.WITHDRAW.PARTICULAR = R.PARAM<MBL.TXN.WITHDRAW.PARTICULAR>
    Y.TOT.WITH.TXN.CODE = R.PARAM<MBL.TXN.WITH.TXN.CODE>
    IF Y.SYS.ID EQ 'TT' THEN
        Y.TRANS.CODE = TT.Contract.getTransactionId()
    END ELSE
        IF Y.SYS.ID EQ 'FT' THEN
            Y.TRANS.TYPE = FT.Contract.getIdTxnType()
            EB.DataAccess.FRead(FN.FT.TYPE, Y.TRANS.TYPE, R.FT.TYPE, F.FT.TYPE, Y.ERR)
            Y.TRANS.CODE = R.FT.TYPE<FT6.TXN.CODE.DR>
        END
    END
    FIND Y.TRANS.CODE IN Y.TOT.WITH.TXN.CODE SETTING TXN.POS1,TXN.POS2,TXN.POS3 THEN
        Y.PARTICULAR = FIELD(Y.WITHDRAW.PARTICULAR,VM,TXN.POS2)
    END ELSE
        RETURN
    END
    IF Y.RECORD.STATUS EQ 'UNAUTH' THEN
        GOSUB PROCESS.WITHDRAW
    END
    IF Y.RECORD.STATUS EQ 'DELETE'  OR Y.RECORD.STATUS EQ 'AUTH-REV' THEN
        GOSUB REMOVE.WITH.OVRR.INFO
    END
    Y.DATA = 'Y.AC.NO=':Y.AC.NO:'*':'Y.TXN.AMT=':Y.TXN.AMT:'*':'Y.TXN.REF=':Y.TXN.REF:'*':'Y.SYS.ID=':Y.SYS.ID
    Y.DIR = 'MBL.DATA'
    Y.FILE.NAME = 'TP'
    OPENSEQ Y.DIR,Y.FILE.NAME TO F.DIR THEN NULL
    WRITESEQ Y.DATA APPEND TO F.DIR ELSE
        CRT "Unable to write"
        CLOSESEQ F.DIR
    END
RETURN

*****************
PROCESS.WITHDRAW:
*****************
***********Write TP Transaction Information Data Into BD.TXN.PROFILE.ENTRY***************
    GOSUB GET.TP.INFO
    IF  NOT(R.TP.REC) THEN
        IF Y.SYS.ID EQ 'FT' THEN
            Y.OR.ID = 'EB-MBL.TP.DEFAULT.WITH.FT'
            EB.DataAccess.FRead(FN.OR,Y.OR.ID,R.OVERRIDE,F.OR,E.OVERRIDE)
            Y.OVERR.ID = R.OVERRIDE<EB.OverrideProcessing.Override.OrMessage>
            Y.OVERR.ID.AC = Y.OVERR.ID:' ': Y.AC.NO
            EB.SystemTables.setText(Y.OVERR.ID.AC)
            Y.OVERRIDE.VAL = EB.SystemTables.getRNew(V-9)
            Y.OVRRD.NO = DCOUNT(Y.OVERRIDE.VAL,VM) + 1
            EB.OverrideProcessing.StoreOverride(Y.OVRRD.NO)
            RETURN
        END
        IF Y.SYS.ID EQ 'TT' THEN
            Y.OR.ID = 'EB-MBL.TP.DEFAULT.WITH.TT'
            EB.DataAccess.FRead(FN.OR,Y.OR.ID,R.OVERRIDE,F.OR,E.OVERRIDE)
            Y.OVERR.ID = R.OVERRIDE<EB.OverrideProcessing.Override.OrMessage>
            Y.OVERR.ID.AC = Y.OVERR.ID:' ': Y.AC.NO
            EB.SystemTables.setText(Y.OVERR.ID.AC)
            Y.OVERRIDE.VAL = EB.SystemTables.getRNew(V-9)
            Y.OVRRD.NO = DCOUNT(Y.OVERRIDE.VAL,VM) + 1
            EB.OverrideProcessing.StoreOverride(Y.OVRRD.NO)
            RETURN
        END
    END ELSE
        GOSUB WITHDRAW.PART
        FINDSTR Y.PARTICULAR IN Y.WITHDRAW.PART SETTING Y.WITH.POS1,Y.WITH.POS2,Y.WITH.POS3 THEN END
          
        IF Y.WITH.POS1 EQ 1 THEN
            Y.WITH.NO.TXN.MON = R.TP.REC<MB.TP.CASH.WDL.NO>
            Y.WITH.MAX.TXN.AMT = R.TP.REC<MB.TP.CASH.WDL.MAX>
            Y.WITH.TOT.AMT = R.TP.REC<MB.TP.CASH.WDL.AMT>
       
            R.TP.ENTRY<MBLTPE.CASH.WITH.OCCPY.TXN> = R.TP.ENTRY<MBLTPE.CASH.WITH.OCCPY.TXN> + 1
            Y.WITH.TOT.TXN = R.TP.ENTRY<MBLTPE.CASH.WITH.OCCPY.TXN>
            IF R.TP.ENTRY<MBLTPE.CASH.WITH.TXN.REF> EQ '' THEN
                R.TP.ENTRY<MBLTPE.CASH.WITH.TXN.REF> = Y.TXN.REF
            END ELSE
                R.TP.ENTRY<MBLTPE.CASH.WITH.TXN.REF> = R.TP.ENTRY<MBLTPE.CASH.WITH.TXN.REF>:'*':Y.TXN.REF
            END
            IF  R.TP.ENTRY<MBLTPE.CASH.WITH.TXN.AMT> EQ '' THEN
                R.TP.ENTRY<MBLTPE.CASH.WITH.TXN.AMT> = Y.TXN.AMT
            END ELSE
                R.TP.ENTRY<MBLTPE.CASH.WITH.TXN.AMT> = R.TP.ENTRY<MBLTPE.CASH.WITH.TXN.AMT>:'*':Y.TXN.AMT
            END
            R.TP.ENTRY<MBLTPE.CASH.WITH.TOT.TXN.AMT> = R.TP.ENTRY<MBLTPE.CASH.WITH.TOT.TXN.AMT> + Y.TXN.AMT
            Y.TOT.TXN.AMT = R.TP.ENTRY<MBLTPE.CASH.WITH.TOT.TXN.AMT>
        END
        IF Y.WITH.POS1 EQ 2 THEN
            Y.WITH.NO.TXN.MON = R.TP.REC<MB.TP.TRF.WDL.NO>
            Y.WITH.MAX.TXN.AMT = R.TP.REC<MB.TP.TRF.WDL.MAX>
            Y.WITH.TOT.AMT = R.TP.REC<MB.TP.TRF.WDL.AMT>
            
            R.TP.ENTRY<MBLTPE.TRF.WITH.OCCPY.TXN> = R.TP.ENTRY<MBLTPE.TRF.WITH.OCCPY.TXN> + 1
            Y.WITH.TOT.TXN = R.TP.ENTRY<MBLTPE.TRF.WITH.OCCPY.TXN>
            IF R.TP.ENTRY<MBLTPE.TRF.WITH.TXN.REF> EQ '' THEN
                R.TP.ENTRY<MBLTPE.TRF.WITH.TXN.REF> = Y.TXN.REF
            END ELSE
                R.TP.ENTRY<MBLTPE.TRF.WITH.TXN.REF> = R.TP.ENTRY<MBLTPE.TRF.WITH.TXN.REF>:'*':Y.TXN.REF
            END
            IF  R.TP.ENTRY<MBLTPE.TRF.WITH.TXN.AMT> EQ '' THEN
                R.TP.ENTRY<MBLTPE.TRF.WITH.TXN.AMT> = Y.TXN.AMT
            END ELSE
                R.TP.ENTRY<MBLTPE.TRF.WITH.TXN.AMT> = R.TP.ENTRY<MBLTPE.TRF.WITH.TXN.AMT>:'*':Y.TXN.AMT
            END
            R.TP.ENTRY<MBLTPE.TRF.WITH.TOT.TXN.AMT> = R.TP.ENTRY<MBLTPE.TRF.WITH.TOT.TXN.AMT> + Y.TXN.AMT
            Y.TOT.TXN.AMT = R.TP.ENTRY<MBLTPE.TRF.WITH.TOT.TXN.AMT>
        END
        IF Y.WITH.POS1 EQ 3 THEN
            Y.WITH.NO.TXN.MON = R.TP.REC<MB.TP.RMT.WDL.NO>
            Y.WITH.MAX.TXN.AMT = R.TP.REC<MB.TP.RMT.WDL.MAX>
            Y.WITH.TOT.AMT = R.TP.REC<MB.TP.RMT.WDL.AMT>
            
            R.TP.ENTRY<MBLTPE.RMT.WITH.OCCPY.TXN> = R.TP.ENTRY<MBLTPE.RMT.WITH.OCCPY.TXN> + 1
            Y.WITH.TOT.TXN = R.TP.ENTRY<MBLTPE.RMT.WITH.OCCPY.TXN>
            IF R.TP.ENTRY<MBLTPE.RMT.WITH.TXN.REF> EQ '' THEN
                R.TP.ENTRY<MBLTPE.RMT.WITH.TXN.REF> = Y.TXN.REF
            END ELSE
                R.TP.ENTRY<MBLTPE.RMT.WITH.TXN.REF> = R.TP.ENTRY<MBLTPE.RMT.WITH.TXN.REF>:'*':Y.TXN.REF
            END
            IF  R.TP.ENTRY<MBLTPE.RMT.WITH.TXN.AMT> EQ '' THEN
                R.TP.ENTRY<MBLTPE.RMT.WITH.TXN.AMT> = Y.TXN.AMT
            END ELSE
                R.TP.ENTRY<MBLTPE.RMT.WITH.TXN.AMT> = R.TP.ENTRY<MBLTPE.RMT.WITH.TXN.AMT>:'*':Y.TXN.AMT
            END
            R.TP.ENTRY<MBLTPE.RMT.WITH.TOT.TXN.AMT> = R.TP.ENTRY<MBLTPE.RMT.WITH.TOT.TXN.AMT> + Y.TXN.AMT
            Y.TOT.TXN.AMT = R.TP.ENTRY<MBLTPE.RMT.WITH.TOT.TXN.AMT>
        END
        IF Y.WITH.POS1 EQ 4 THEN
            Y.WITH.NO.TXN.MON = R.TP.REC<MB.TP.IMP.WDL.NO>
            Y.WITH.MAX.TXN.AMT = R.TP.REC<MB.TP.IMP.WDL.MAX>
            Y.WITH.TOT.AMT = R.TP.REC<MB.TP.IMP.WDL.AMT>
            
            R.TP.ENTRY<MBLTPE.IMP.WITH.OCCPY.TXN> = R.TP.ENTRY<MBLTPE.IMP.WITH.OCCPY.TXN> + 1
            Y.WITH.TOT.TXN = R.TP.ENTRY<MBLTPE.IMP.WITH.OCCPY.TXN>
            IF R.TP.ENTRY<MBLTPE.IMP.WITH.TXN.REF> EQ '' THEN
                R.TP.ENTRY<MBLTPE.IMP.WITH.TXN.REF> = Y.TXN.REF
            END ELSE
                R.TP.ENTRY<MBLTPE.IMP.WITH.TXN.REF> = R.TP.ENTRY<MBLTPE.IMP.WITH.TXN.REF>:'*':Y.TXN.REF
            END
            IF  R.TP.ENTRY<MBLTPE.IMP.WITH.TXN.AMT> EQ '' THEN
                R.TP.ENTRY<MBLTPE.IMP.WITH.TXN.AMT> = Y.TXN.AMT
            END ELSE
                R.TP.ENTRY<MBLTPE.IMP.WITH.TXN.AMT> = R.TP.ENTRY<MBLTPE.IMP.WITH.TXN.AMT>:'*':Y.TXN.AMT
            END
            R.TP.ENTRY<MBLTPE.IMP.WITH.TOT.TXN.AMT> = R.TP.ENTRY<MBLTPE.IMP.WITH.TOT.TXN.AMT> + Y.TXN.AMT
            Y.TOT.TXN.AMT = R.TP.ENTRY<MBLTPE.IMP.WITH.TOT.TXN.AMT>
        END
        IF Y.WITH.POS1 EQ 5 THEN
            Y.WITH.NO.TXN.MON = R.TP.REC<MB.TP.BOA.WDL.NO>
            Y.WITH.MAX.TXN.AMT = R.TP.REC<MB.TP.BOA.WDL.MAX>
            Y.WITH.TOT.AMT = R.TP.REC<MB.TP.BOA.WDL.AMT>
            
            R.TP.ENTRY<MBLTPE.BOA.WITH.OCCPY.TXN> = R.TP.ENTRY<MBLTPE.BOA.WITH.OCCPY.TXN> + 1
            Y.WITH.TOT.TXN = R.TP.ENTRY<MBLTPE.BOA.WITH.OCCPY.TXN>
            IF R.TP.ENTRY<MBLTPE.BOA.WITH.TXN.REF> EQ '' THEN
                R.TP.ENTRY<MBLTPE.BOA.WITH.TXN.REF> = Y.TXN.REF
            END ELSE
                R.TP.ENTRY<MBLTPE.BOA.WITH.TXN.REF> = R.TP.ENTRY<MBLTPE.BOA.WITH.TXN.REF>:'*':Y.TXN.REF
            END
            IF  R.TP.ENTRY<MBLTPE.BOA.WITH.TXN.AMT> EQ '' THEN
                R.TP.ENTRY<MBLTPE.BOA.WITH.TXN.AMT> = Y.TXN.AMT
            END ELSE
                R.TP.ENTRY<MBLTPE.BOA.WITH.TXN.AMT> = R.TP.ENTRY<MBLTPE.BOA.WITH.TXN.AMT>:'*':Y.TXN.AMT
            END
            R.TP.ENTRY<MBLTPE.BOA.WITH.TOT.TXN.AMT> = R.TP.ENTRY<MBLTPE.BOA.WITH.TOT.TXN.AMT> + Y.TXN.AMT
            Y.TOT.TXN.AMT = R.TP.ENTRY<MBLTPE.BOA.WITH.TOT.TXN.AMT>
        END
        IF Y.WITH.POS1 EQ 6 THEN
            Y.WITH.NO.TXN.MON = R.TP.REC<MB.TP.OTH.WDL.NO>
            Y.WITH.MAX.TXN.AMT = R.TP.REC<MB.TP.OTH.WDL.MAX>
            Y.WITH.TOT.AMT = R.TP.REC<MB.TP.OTH.WDL.AMT>
            
            R.TP.ENTRY<MBLTPE.OTH.WITH.OCCPY.TXN> = R.TP.ENTRY<MBLTPE.OTH.WITH.OCCPY.TXN> + 1
            Y.WITH.TOT.TXN = R.TP.ENTRY<MBLTPE.OTH.WITH.OCCPY.TXN>
            IF R.TP.ENTRY<MBLTPE.OTH.WITH.TXN.REF> EQ '' THEN
                R.TP.ENTRY<MBLTPE.OTH.WITH.TXN.REF> = Y.TXN.REF
            END ELSE
                R.TP.ENTRY<MBLTPE.OTH.WITH.TXN.REF> = R.TP.ENTRY<MBLTPE.OTH.WITH.TXN.REF>:'*':Y.TXN.REF
            END
            IF  R.TP.ENTRY<MBLTPE.OTH.WITH.TXN.AMT> EQ '' THEN
                R.TP.ENTRY<MBLTPE.OTH.WITH.TXN.AMT> = Y.TXN.AMT
            END ELSE
                R.TP.ENTRY<MBLTPE.OTH.WITH.TXN.AMT> = R.TP.ENTRY<MBLTPE.OTH.WITH.TXN.AMT>:'*':Y.TXN.AMT
            END
            R.TP.ENTRY<MBLTPE.OTH.WITH.TOT.TXN.AMT> = R.TP.ENTRY<MBLTPE.OTH.WITH.TOT.TXN.AMT> + Y.TXN.AMT
            Y.TOT.TXN.AMT = R.TP.ENTRY<MBLTPE.OTH.WITH.TOT.TXN.AMT>
        END
        WRITE R.TP.ENTRY ON F.TP.ENTRY,Y.TP.ENTRY.ID
        GOSUB WRITE.WITH.OVRR.MSG
    END
RETURN

************
GET.TP.INFO:
************

***********Read TP Record***************
    Y.TP.REC.ID = Y.AC.NO
    EB.DataAccess.FRead(FN.TP,Y.TP.REC.ID,R.TP.REC,F.TP,TP.ERR)
    IF R.TP.REC NE '' THEN
        Y.TP.ENTRY.ID = Y.AC.NO:'.':EB.SystemTables.getToday()[1,6]
        EB.DataAccess.FRead(FN.TP.ENTRY,Y.TP.ENTRY.ID,R.TP.ENTRY,F.TP.ENTRY,TP.ERR.ENTRY)
        Y.WITH.E.PART = R.TP.ENTRY<MBLTPE.HDR.WDL.CSH>
        IF Y.WITH.E.PART NE '' THEN
            RETURN
        END ELSE
            R.TP.ENTRY<MBLTPE.HDR.WDL.CSH> = 'Cash With. (Inc of Online & ATM)'
            R.TP.ENTRY<MBLTPE.HDR.WDL.TRF> = 'Transfer/Payment by Instruments'
            R.TP.ENTRY<MBLTPE.HDR.WDL.RMT> = 'Foreign Outward Remittance'
            R.TP.ENTRY<MBLTPE.HDR.WDL.IMP> = 'Settlement of Import Payment'
            R.TP.ENTRY<MBLTPE.HDR.WDL.BOA> = 'Deposit/Transfer to BO Account'
            R.TP.ENTRY<MBLTPE.HDR.WDL.OTH> = 'Other Withdrawal'
            R.TP.ENTRY<MBLTPE.CO.CODE> = EB.SystemTables.getIdCompany()
        END
    END
RETURN
**************
WITHDRAW.PART:
**************
    Y.WITHDRAW.PART = ''
    IF R.TP.REC<MB.TP.CASH.WDL.NO> THEN
        Y.WITHDRAW.PART = 'Cash With. (Inc of Online & ATM)'
    END
    IF R.TP.REC<MB.TP.TRF.WDL.NO> THEN
        Y.WITHDRAW.PART = Y.WITHDRAW.PART :FM:'Transfer/Payment by Instruments'
    END
    IF R.TP.REC<MB.TP.RMT.WDL.NO> THEN
        Y.WITHDRAW.PART = Y.WITHDRAW.PART:FM:'Foreign Outward Remittance'
    END

    IF R.TP.REC<MB.TP.IMP.WDL.NO> THEN
        Y.WITHDRAW.PART = Y.WITHDRAW.PART:FM:'Settlement of Import Payment'
    END

    IF R.TP.REC<MB.TP.BOA.WDL.NO> THEN
        Y.WITHDRAW.PART = Y.WITHDRAW.PART:FM:'Deposit/Transfer to BO Account'
    END

    IF R.TP.REC<MB.TP.OTH.WDL.NO> THEN
        Y.WITHDRAW.PART = Y.WITHDRAW.PART:FM:'Other Withdrawal'
    END
RETURN
********************
WRITE.WITH.OVRR.MSG:
********************
    IF Y.SYS.ID EQ 'FT' THEN
        IF Y.TXN.AMT GT Y.WITH.MAX.TXN.AMT THEN
            Y.OR.ID = 'EB-MBL.TP.AMT.WITH.FT'
            EB.DataAccess.FRead(FN.OR, Y.OR.ID, R.OVERRIDE, F.OR, E.OVERRIDE)
            Y.OVERR.ID = R.OVERRIDE<EB.OverrideProcessing.Override.OrMessage>
            Y.OVERR.ID.AC = Y.OVERR.ID:' ':Y.AC.NO
            EB.SystemTables.setText(Y.OVERR.ID.AC)
            Y.OVERRIDE.VAL = EB.SystemTables.getRNew(V-9)
            Y.OVRRD.NO = DCOUNT(Y.OVERRIDE.VAL,VM) + 1
            EB.OverrideProcessing.StoreOverride(Y.OVRRD.NO)
        END

        IF Y.WITH.TOT.TXN GT Y.WITH.NO.TXN.MON THEN
            Y.OR.ID = 'EB-MBL.TP.TXN.WITH.FT'
            EB.DataAccess.FRead(FN.OR, Y.OR.ID, R.OVERRIDE, F.OR, E.OVERRIDE)
            Y.OVERR.ID = R.OVERRIDE<EB.OverrideProcessing.Override.OrMessage>
            Y.OVERR.ID.AC = Y.OVERR.ID:' ':Y.AC.NO
            EB.SystemTables.setText(Y.OVERR.ID.AC)
            Y.OVERRIDE.VAL = EB.SystemTables.getRNew(V-9)
            Y.OVRRD.NO = DCOUNT(Y.OVERRIDE.VAL,VM) + 1
            EB.OverrideProcessing.StoreOverride(Y.OVRRD.NO)
        END
        IF Y.TOT.TXN.AMT GT Y.WITH.TOT.AMT THEN
            Y.OR.ID = 'EB-MBL.TP.TOT.AMT.WITH.FT'
            EB.DataAccess.FRead(FN.OR, Y.OR.ID, R.OVERRIDE, F.OR, E.OVERRIDE)
            Y.OVERR.ID = R.OVERRIDE<EB.OverrideProcessing.Override.OrMessage>
            Y.OVERR.ID.AC = Y.OVERR.ID:' ':Y.AC.NO
            EB.SystemTables.setText(Y.OVERR.ID.AC)
            Y.OVERRIDE.VAL = EB.SystemTables.getRNew(V-9)
            Y.OVRRD.NO = DCOUNT(Y.OVERRIDE.VAL,VM) + 1
            EB.OverrideProcessing.StoreOverride(Y.OVRRD.NO)
        END
    END
    IF Y.SYS.ID EQ 'TT' THEN
        IF Y.TXN.AMT GT Y.WITH.MAX.TXN.AMT THEN
            Y.OR.ID = 'EB-MBL.TP.AMT.WITH.TT'
            EB.DataAccess.FRead(FN.OR, Y.OR.ID, R.OVERRIDE, F.OR, E.OVERRIDE)
            Y.OVERR.ID = R.OVERRIDE<EB.OverrideProcessing.Override.OrMessage>
            Y.OVERR.ID.AC = Y.OVERR.ID:' ':Y.AC.NO
            EB.SystemTables.setText(Y.OVERR.ID.AC)
            Y.OVERRIDE.VAL = EB.SystemTables.getRNew(V-9)
            Y.OVRRD.NO = DCOUNT(Y.OVERRIDE.VAL,VM) + 1
            EB.OverrideProcessing.StoreOverride(Y.OVRRD.NO)
        END

        IF Y.WITH.TOT.TXN GT Y.WITH.NO.TXN.MON THEN
            Y.OR.ID = 'EB-MBL.TP.TXN.WITH.TT'
            EB.DataAccess.FRead(FN.OR, Y.OR.ID, R.OVERRIDE, F.OR, E.OVERRIDE)
            Y.OVERR.ID = R.OVERRIDE<EB.OverrideProcessing.Override.OrMessage>
            Y.OVERR.ID.AC = Y.OVERR.ID:' ':Y.AC.NO
            EB.SystemTables.setText(Y.OVERR.ID.AC)
            Y.OVERRIDE.VAL = EB.SystemTables.getRNew(V-9)
            Y.OVRRD.NO = DCOUNT(Y.OVERRIDE.VAL,VM) + 1
            EB.OverrideProcessing.StoreOverride(Y.OVRRD.NO)
        END
        IF Y.TOT.TXN.AMT GT Y.WITH.TOT.AMT THEN
            Y.OR.ID = 'EB-MBL.TP.TOT.AMT.WITH.TT'
            EB.DataAccess.FRead(FN.OR, Y.OR.ID, R.OVERRIDE, F.OR, E.OVERRIDE)
            Y.OVERR.ID = R.OVERRIDE<EB.OverrideProcessing.Override.OrMessage>
            Y.OVERR.ID.AC = Y.OVERR.ID:' ':Y.AC.NO
            EB.SystemTables.setText(Y.OVERR.ID.AC)
            Y.OVERRIDE.VAL = EB.SystemTables.getRNew(V-9)
            Y.OVRRD.NO = DCOUNT(Y.OVERRIDE.VAL,VM) + 1
            EB.OverrideProcessing.StoreOverride(Y.OVRRD.NO)
        END
    END
    Y.DATA = 'Y.AC.NO=':Y.AC.NO:'*':'Y.WITH.MAX.TXN.AMT=':Y.WITH.MAX.TXN.AMT:'*':'Y.WITH.TOT.TXN=':Y.WITH.TOT.TXN:'*':'Y.WITH.NO.TXN.MON=':Y.WITH.NO.TXN.MON:'*':'Y.TOT.TXN.AMT=':Y.TOT.TXN.AMT:'*':'Y.WITH.TOT.AMT=':Y.WITH.TOT.AMT
    Y.DIR = 'MBL.DATA'
    Y.FILE.NAME = 'TP'
    OPENSEQ Y.DIR,Y.FILE.NAME TO F.DIR THEN NULL
    WRITESEQ Y.DATA APPEND TO F.DIR ELSE
        CRT "Unable to write"
        CLOSESEQ F.DIR
    END
RETURN
**********************
REMOVE.WITH.OVRR.INFO:
**********************
    Y.TP.ENTRY.ID = Y.AC.NO:'.':EB.SystemTables.getToday()[1,6]
    EB.DataAccess.FRead(FN.TP.ENTRY,Y.TP.ENTRY.ID,R.TP.ENTRY,F.TP.ENTRY,TP.ERR.ENTRY)
    EB.DataAccess.FRead(FN.TP,Y.AC.NO,R.TP.REC,F.TP,TP.ERR)
    IF R.TP.ENTRY THEN
        GOSUB WITHDRAW.PART
        FINDSTR Y.PARTICULAR  IN Y.WITHDRAW.PART SETTING Y.WITH.POS1,Y.WITH.POS2,Y.WITH.POS3 ELSE NULL

        IF Y.WITH.POS1 EQ 1 THEN
            Y.OCCPY.TXNS = R.TP.ENTRY<MBLTPE.CASH.WITH.OCCPY.TXN> -1
            FINDSTR Y.TXN.REF:'*' IN R.TP.ENTRY<MBLTPE.CASH.WITH.TXN.REF> SETTING Y.POS1 ELSE NULL
            IF Y.POS1 THEN
                Y.TXN.REF.REPLACE.VALUE = EREPLACE(R.TP.ENTRY<MBLTPE.CASH.WITH.TXN.REF>,Y.TXN.REF:'*','')
                Y.TXN.AMT.REPLACE.VALUE = EREPLACE(R.TP.ENTRY<MBLTPE.CASH.WITH.TXN.AMT>,Y.TXN.AMT:'*','')
            END ELSE
                FINDSTR '*' IN R.TP.ENTRY<MBLTPE.CASH.WITH.TXN.REF> SETTING Y.DM.POS1 ELSE NULL
                IF NOT(Y.DM.POS1) THEN
                    Y.TXN.REF.REPLACE.VALUE = EREPLACE(R.TP.ENTRY<MBLTPE.CASH.WITH.TXN.REF>,Y.TXN.REF,'')
                    Y.TXN.AMT.REPLACE.VALUE = EREPLACE(R.TP.ENTRY<MBLTPE.CASH.WITH.TXN.AMT>,Y.TXN.AMT,'')
                END ELSE
                    Y.TXN.REF.REPLACE.VALUE = EREPLACE(R.TP.ENTRY<MBLTPE.CASH.WITH.TXN.REF>,'*':Y.TXN.REF,'')
                    Y.TXN.AMT.REPLACE.VALUE = EREPLACE(R.TP.ENTRY<MBLTPE.CASH.WITH.TXN.AMT>,'*':Y.TXN.AMT,'')
                END
            END
            IF Y.TXN.REF.REPLACE.VALUE NE 0 AND Y.TXN.AMT.REPLACE.VALUE NE 0 THEN
                R.TP.ENTRY<MBLTPE.CASH.WITH.OCCPY.TXN> = Y.OCCPY.TXNS
                R.TP.ENTRY<MBLTPE.CASH.WITH.TXN.REF> = Y.TXN.REF.REPLACE.VALUE
                R.TP.ENTRY<MBLTPE.CASH.WITH.TXN.AMT> = Y.TXN.AMT.REPLACE.VALUE
                R.TP.ENTRY<MBLTPE.CASH.WITH.TOT.TXN.AMT> = R.TP.ENTRY<MBLTPE.CASH.WITH.TOT.TXN.AMT> - Y.TXN.AMT
                Y.TOT.AMT =  R.TP.ENTRY<MBLTPE.CASH.WITH.TOT.TXN.AMT>
            END
        END
        IF Y.WITH.POS1 EQ 2 THEN
            Y.OCCPY.TXNS = R.TP.ENTRY<MBLTPE.TRF.WITH.OCCPY.TXN> -1
            FINDSTR Y.TXN.REF:'*' IN R.TP.ENTRY<MBLTPE.TRF.WITH.TXN.REF> SETTING Y.POS1 ELSE NULL
            IF Y.POS1 THEN
                Y.TXN.REF.REPLACE.VALUE = EREPLACE(R.TP.ENTRY<MBLTPE.TRF.WITH.TXN.REF>,Y.TXN.REF:'*','')
                Y.TXN.AMT.REPLACE.VALUE = EREPLACE(R.TP.ENTRY<MBLTPE.TRF.WITH.TXN.AMT>,Y.TXN.AMT:'*','')
            END ELSE
                FINDSTR '*' IN R.TP.ENTRY<MBLTPE.TRF.WITH.TXN.REF> SETTING Y.DM.POS1 ELSE NULL
                IF NOT(Y.DM.POS1) THEN
                    Y.TXN.REF.REPLACE.VALUE = EREPLACE(R.TP.ENTRY<MBLTPE.TRF.WITH.TXN.REF>,Y.TXN.REF,'')
                    Y.TXN.AMT.REPLACE.VALUE = EREPLACE(R.TP.ENTRY<MBLTPE.TRF.WITH.TXN.AMT>,Y.TXN.AMT,'')
                END ELSE
                    Y.TXN.REF.REPLACE.VALUE = EREPLACE(R.TP.ENTRY<MBLTPE.TRF.WITH.TXN.REF>,'*':Y.TXN.REF,'')
                    Y.TXN.AMT.REPLACE.VALUE = EREPLACE(R.TP.ENTRY<MBLTPE.TRF.WITH.TXN.AMT>,'*':Y.TXN.AMT,'')
                END
            END
            IF Y.TXN.REF.REPLACE.VALUE NE 0 AND Y.TXN.AMT.REPLACE.VALUE NE 0 THEN
                R.TP.ENTRY<MBLTPE.TRF.WITH.OCCPY.TXN> = Y.OCCPY.TXNS
                R.TP.ENTRY<MBLTPE.TRF.WITH.TXN.REF> = Y.TXN.REF.REPLACE.VALUE
                R.TP.ENTRY<MBLTPE.TRF.WITH.TXN.AMT> = Y.TXN.AMT.REPLACE.VALUE
                R.TP.ENTRY<MBLTPE.TRF.WITH.TOT.TXN.AMT> = R.TP.ENTRY<MBLTPE.TRF.WITH.TOT.TXN.AMT> - Y.TXN.AMT
            END
        END
        IF Y.WITH.POS1 EQ 3 THEN
            Y.OCCPY.TXNS = R.TP.ENTRY<MBLTPE.RMT.WITH.OCCPY.TXN> -1
            FINDSTR Y.TXN.REF:'*' IN R.TP.ENTRY<MBLTPE.RMT.WITH.TXN.REF> SETTING Y.POS1 ELSE NULL
            IF Y.POS1 THEN
                Y.TXN.REF.REPLACE.VALUE = EREPLACE(R.TP.ENTRY<MBLTPE.RMT.WITH.TXN.REF>,Y.TXN.REF:'*','')
                Y.TXN.AMT.REPLACE.VALUE = EREPLACE(R.TP.ENTRY<MBLTPE.RMT.WITH.TXN.AMT>,Y.TXN.AMT:'*','')
            END ELSE
                FINDSTR '*' IN R.TP.ENTRY<MBLTPE.RMT.WITH.TXN.REF> SETTING Y.DM.POS1 ELSE NULL
                IF NOT(Y.DM.POS1) THEN
                    Y.TXN.REF.REPLACE.VALUE = EREPLACE(R.TP.ENTRY<MBLTPE.RMT.WITH.TXN.REF>,Y.TXN.REF,'')
                    Y.TXN.AMT.REPLACE.VALUE = EREPLACE(R.TP.ENTRY<MBLTPE.RMT.WITH.TXN.AMT>,Y.TXN.AMT,'')
                END ELSE
                    Y.TXN.REF.REPLACE.VALUE = EREPLACE(R.TP.ENTRY<MBLTPE.RMT.WITH.TXN.REF>,'*':Y.TXN.REF,'')
                    Y.TXN.AMT.REPLACE.VALUE = EREPLACE(R.TP.ENTRY<MBLTPE.RMT.WITH.TXN.AMT>,'*':Y.TXN.AMT,'')
                END
            END
            IF Y.TXN.REF.REPLACE.VALUE NE 0 AND Y.TXN.AMT.REPLACE.VALUE NE 0 THEN
                R.TP.ENTRY<MBLTPE.RMT.WITH.OCCPY.TXN> = Y.OCCPY.TXNS
                R.TP.ENTRY<MBLTPE.RMT.WITH.TXN.REF> = Y.TXN.REF.REPLACE.VALUE
                R.TP.ENTRY<MBLTPE.RMT.WITH.TXN.AMT> = Y.TXN.AMT.REPLACE.VALUE
                R.TP.ENTRY<MBLTPE.RMT.WITH.TOT.TXN.AMT> = R.TP.ENTRY<MBLTPE.RMT.WITH.TOT.TXN.AMT> - Y.TXN.AMT
            END
        END
        IF Y.WITH.POS1 EQ 4 THEN
            Y.OCCPY.TXNS = R.TP.ENTRY<MBLTPE.IMP.WITH.OCCPY.TXN> -1
            FINDSTR Y.TXN.REF:'*' IN R.TP.ENTRY<MBLTPE.IMP.WITH.TXN.REF> SETTING Y.POS1 ELSE NULL
            IF Y.POS1 THEN
                Y.TXN.REF.REPLACE.VALUE = EREPLACE(R.TP.ENTRY<MBLTPE.IMP.WITH.TXN.REF>,Y.TXN.REF:'*','')
                Y.TXN.AMT.REPLACE.VALUE = EREPLACE(R.TP.ENTRY<MBLTPE.IMP.WITH.TXN.AMT>,Y.TXN.AMT:'*','')
            END ELSE
                FINDSTR '*' IN R.TP.ENTRY<MBLTPE.IMP.WITH.TXN.REF> SETTING Y.DM.POS1 ELSE NULL
                IF NOT(Y.DM.POS1) THEN
                    Y.TXN.REF.REPLACE.VALUE = EREPLACE(R.TP.ENTRY<MBLTPE.IMP.WITH.TXN.REF>,Y.TXN.REF,'')
                    Y.TXN.AMT.REPLACE.VALUE = EREPLACE(R.TP.ENTRY<MBLTPE.IMP.WITH.TXN.AMT>,Y.TXN.AMT,'')
                END ELSE
                    Y.TXN.REF.REPLACE.VALUE = EREPLACE(R.TP.ENTRY<MBLTPE.IMP.WITH.TXN.REF>,'*':Y.TXN.REF,'')
                    Y.TXN.AMT.REPLACE.VALUE = EREPLACE(R.TP.ENTRY<MBLTPE.IMP.WITH.TXN.AMT>,'*':Y.TXN.AMT,'')
                END
            END
            IF Y.TXN.REF.REPLACE.VALUE NE 0 AND Y.TXN.AMT.REPLACE.VALUE NE 0 THEN
                R.TP.ENTRY<MBLTPE.IMP.WITH.OCCPY.TXN> = Y.OCCPY.TXNS
                R.TP.ENTRY<MBLTPE.IMP.WITH.TXN.REF> = Y.TXN.REF.REPLACE.VALUE
                R.TP.ENTRY<MBLTPE.IMP.WITH.TXN.AMT> = Y.TXN.AMT.REPLACE.VALUE
                R.TP.ENTRY<MBLTPE.IMP.WITH.TOT.TXN.AMT> = R.TP.ENTRY<MBLTPE.IMP.WITH.TOT.TXN.AMT> - Y.TXN.AMT
            END
        END
        IF Y.WITH.POS1 EQ 5 THEN
            Y.OCCPY.TXNS = R.TP.ENTRY<MBLTPE.BOA.WITH.OCCPY.TXN> -1
            FINDSTR Y.TXN.REF:'*' IN R.TP.ENTRY<MBLTPE.BOA.WITH.TXN.REF> SETTING Y.POS1 ELSE NULL
            IF Y.POS1 THEN
                Y.TXN.REF.REPLACE.VALUE = EREPLACE(R.TP.ENTRY<MBLTPE.BOA.WITH.TXN.REF>,Y.TXN.REF:'*','')
                Y.TXN.AMT.REPLACE.VALUE = EREPLACE(R.TP.ENTRY<MBLTPE.BOA.WITH.TXN.AMT>,Y.TXN.AMT:'*','')
            END ELSE
                FINDSTR '*' IN R.TP.ENTRY<MBLTPE.BOA.WITH.TXN.REF> SETTING Y.DM.POS1 ELSE NULL
                IF NOT(Y.DM.POS1) THEN
                    Y.TXN.REF.REPLACE.VALUE = EREPLACE(R.TP.ENTRY<MBLTPE.BOA.WITH.TXN.REF>,Y.TXN.REF,'')
                    Y.TXN.AMT.REPLACE.VALUE = EREPLACE(R.TP.ENTRY<MBLTPE.BOA.WITH.TXN.AMT>,Y.TXN.AMT,'')
                END ELSE
                    Y.TXN.REF.REPLACE.VALUE = EREPLACE(R.TP.ENTRY<MBLTPE.BOA.WITH.TXN.REF>,'*':Y.TXN.REF,'')
                    Y.TXN.AMT.REPLACE.VALUE = EREPLACE(R.TP.ENTRY<MBLTPE.BOA.WITH.TXN.AMT>,'*':Y.TXN.AMT,'')
                END
            END
            IF Y.TXN.REF.REPLACE.VALUE NE 0 AND Y.TXN.AMT.REPLACE.VALUE NE 0 THEN
                R.TP.ENTRY<MBLTPE.BOA.WITH.OCCPY.TXN> = Y.OCCPY.TXNS
                R.TP.ENTRY<MBLTPE.BOA.WITH.TXN.REF> = Y.TXN.REF.REPLACE.VALUE
                R.TP.ENTRY<MBLTPE.BOA.WITH.TXN.AMT> = Y.TXN.AMT.REPLACE.VALUE
                R.TP.ENTRY<MBLTPE.BOA.WITH.TOT.TXN.AMT> = R.TP.ENTRY<MBLTPE.BOA.WITH.TOT.TXN.AMT> - Y.TXN.AMT
            END
        END
        IF Y.WITH.POS1 EQ 6 THEN
            Y.OCCPY.TXNS = R.TP.ENTRY<MBLTPE.OTH.WITH.OCCPY.TXN> -1
            FINDSTR Y.TXN.REF:'*' IN R.TP.ENTRY<MBLTPE.OTH.WITH.TXN.REF> SETTING Y.POS1 ELSE NULL
            IF Y.POS1 THEN
                Y.TXN.REF.REPLACE.VALUE = EREPLACE(R.TP.ENTRY<MBLTPE.OTH.WITH.TXN.REF>,Y.TXN.REF:'*','')
                Y.TXN.AMT.REPLACE.VALUE = EREPLACE(R.TP.ENTRY<MBLTPE.OTH.WITH.TXN.AMT>,Y.TXN.AMT:'*','')
            END ELSE
                FINDSTR '*' IN R.TP.ENTRY<MBLTPE.OTH.WITH.TXN.REF> SETTING Y.DM.POS1 ELSE NULL
                IF NOT(Y.DM.POS1) THEN
                    Y.TXN.REF.REPLACE.VALUE = EREPLACE(R.TP.ENTRY<MBLTPE.OTH.WITH.TXN.REF>,Y.TXN.REF,'')
                    Y.TXN.AMT.REPLACE.VALUE = EREPLACE(R.TP.ENTRY<MBLTPE.OTH.WITH.TXN.AMT>,Y.TXN.AMT,'')
                END ELSE
                    Y.TXN.REF.REPLACE.VALUE = EREPLACE(R.TP.ENTRY<MBLTPE.OTH.WITH.TXN.REF>,'*':Y.TXN.REF,'')
                    Y.TXN.AMT.REPLACE.VALUE = EREPLACE(R.TP.ENTRY<MBLTPE.OTH.WITH.TXN.AMT>,'*':Y.TXN.AMT,'')
                END
            END
            IF Y.TXN.REF.REPLACE.VALUE NE 0 AND Y.TXN.AMT.REPLACE.VALUE NE 0 THEN
                R.TP.ENTRY<MBLTPE.OTH.WITH.OCCPY.TXN> = Y.OCCPY.TXNS
                R.TP.ENTRY<MBLTPE.OTH.WITH.TXN.REF> = Y.TXN.REF.REPLACE.VALUE
                R.TP.ENTRY<MBLTPE.OTH.WITH.TXN.AMT> = Y.TXN.AMT.REPLACE.VALUE
                R.TP.ENTRY<MBLTPE.OTH.WITH.TOT.TXN.AMT> = R.TP.ENTRY<MBLTPE.OTH.WITH.TOT.TXN.AMT> - Y.TXN.AMT
            END
        END
        WRITE R.TP.ENTRY ON F.TP.ENTRY,Y.TP.ENTRY.ID
    END
RETURN
END
    