* @ValidationCode : MjoxMzgwNjM3NDpDcDEyNTI6MTU3MzAyMDc5MDk5MTpERUxMOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjE3X0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 06 Nov 2019 12:13:10
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : DELL
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R17_AMR.0
SUBROUTINE TF.MBL.A.BTB.UPDT.AMND.SCT.JOB.REG
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.BD.BTB.JOB.REGISTER
    $INSERT I_F.BD.SCT.CAPTURE
*-----------------------------------------------------------------------------
    $USING EB.SystemTables
    $USING EB.DataAccess
    $USING EB.TransactionControl
    
    GOSUB INIT
    GOSUB OPENFILES
    GOSUB PROCESS
RETURN

INIT:
    FN.BTB.JOB.REGISTER = 'F.BD.BTB.JOB.REGISTER'
    F.BTB.JOB.REGISTER = ''
    Y.JOB.REG.ID = EB.SystemTables.getRNew(SCT.BTB.JOB.NO)
RETURN

OPENFILES:
    EB.DataAccess.Opf(FN.BTB.JOB.REGISTER, F.BTB.JOB.REGISTER)
RETURN

PROCESS:
    Y.SCT.CURRENCY = EB.SystemTables.getRNew(SCT.CURRENCY)
    Y.SCT.ISS.DATE = EB.SystemTables.getRNew(SCT.CONTRACT.DATE)
    Y.SCT.CONT.AMT = EB.SystemTables.getRNew(SCT.CONTRACT.AMT)
    Y.SCT.CONT.AMT.OLD = EB.SystemTables.getROld(SCT.CONTRACT.AMT)
    Y.JOB.EXC.RATE = EB.SystemTables.getRNew(SCT.JOB.EXCHG.RATE)
    Y.CONV.SCT.CONT.AMT = Y.SCT.CONT.AMT * Y.JOB.EXC.RATE
    Y.CONV.SCT.CONT.AMT.DIFF = (Y.SCT.CONT.AMT - Y.SCT.CONT.AMT.OLD) * Y.JOB.EXC.RATE
    Y.SHIPMENT.DATE = EB.SystemTables.getRNew(SCT.SHIPMENT.DATE)
    Y.EXPIRY.DATE = EB.SystemTables.getRNew(SCT.EXPIRY.DATE)
    Y.NET.FOB.AMT = EB.SystemTables.getRNew(SCT.NET.FOB.VALUE)
    Y.NET.FOB.AMT.OLD = EB.SystemTables.getROld(SCT.NET.FOB.VALUE)
    Y.CONV.NET.FOB.AMT = Y.NET.FOB.AMT * Y.JOB.EXC.RATE
    Y.CONV.NET.FOB.AMT.DIFF = (Y.NET.FOB.AMT - Y.NET.FOB.AMT.OLD) * Y.JOB.EXC.RATE
    Y.CONT.BTB.ENT.AMT = EB.SystemTables.getRNew(SCT.BTB.ENT.AMT)
    Y.CONT.PC.ENT.AMT = EB.SystemTables.getRNew(SCT.PCECC.ENT.AMT)
    
    IF EB.SystemTables.getRNew(SCT.RECORD.STATUS) NE 'RNAU' THEN
*This portion is for INAU,INAO record
*       Y.OLD.JOB.EXC.RATE = EB.SystemTables.getROld(SCT.JOB.EXCHG.RATE)
*       Y.OLD.BTB.ENT.AMT = Y.OLD.JOB.EXC.RATE * EB.SystemTables.getROld(SCT.BTB.ENT.AMT)
*       Y.OLD.PC.ENT.AMT = Y.OLD.JOB.EXC.RATE * EB.SystemTables.getROld(SCT.PCECC.ENT.AMT)
        Y.OLD.BTB.ENT.AMT = EB.SystemTables.getROld(SCT.BTB.ENT.AMT)
        Y.OLD.PC.ENT.AMT = EB.SystemTables.getROld(SCT.PCECC.ENT.AMT)

*       Y.NEW.JOB.EXC.RATE = EB.SystemTables.getRNew(SCT.JOB.EXCHG.RATE)
*       Y.NEW.BTB.ENT.AMT = Y.NEW.JOB.EXC.RATE * EB.SystemTables.getRNew(SCT.BTB.ENT.AMT)
*       Y.NEW.PC.ENT.AMT = Y.NEW.JOB.EXC.RATE * EB.SystemTables.getRNew(SCT.PCECC.ENT.AMT)
        Y.NEW.BTB.ENT.AMT = EB.SystemTables.getRNew(SCT.BTB.ENT.AMT)
        Y.NEW.PC.ENT.AMT = EB.SystemTables.getRNew(SCT.PCECC.ENT.AMT)

        Y.BTB.ENT.AMT = DROUND((Y.NEW.BTB.ENT.AMT - Y.OLD.BTB.ENT.AMT),2)
        Y.PC.ENT.AMT = DROUND((Y.NEW.PC.ENT.AMT - Y.OLD.PC.ENT.AMT),2)

        EB.DataAccess.FRead(FN.BTB.JOB.REGISTER,Y.JOB.REG.ID, REC.JOB.REG, F.BTB.JOB.REGISTER, ERR.JOB.SEQREG)
        Y.CONTRACT.NO = EB.SystemTables.getIdNew()
        Y.ALL.JOB.CONT.REFNO = REC.JOB.REG<BTB.JOB.CONT.REFNO>
        FIND Y.CONTRACT.NO IN Y.ALL.JOB.CONT.REFNO SETTING POS1,POS2,POS3 ELSE NULL
        GOSUB UPDATE.JOB.REG
    END ELSE

*This posrtion is for RNAU record
*       Y.JOB.EXC.RATE = EB.SystemTables.getRNew(SCT.JOB.EXCHG.RATE)
*       Y.BTB.ENT.AMT = Y.JOB.EXC.RATE * EB.SystemTables.getRNew(SCT.BTB.ENT.AMT)
*       Y.PC.ENT.AMT = Y.JOB.EXC.RATE * EB.SystemTables.getRNew(SCT.PCECC.ENT.AMT)
 
        EB.DataAccess.FRead(FN.BTB.JOB.REGISTER,Y.JOB.REG.ID, REC.JOB.REG, F.BTB.JOB.REGISTER, ERR.JOB.SEQREG)
        Y.CONTRACT.NO = EB.SystemTables.getIdNew()
        Y.ALL.JOB.CONT.REFNO = REC.JOB.REG<BTB.JOB.CONT.REFNO>
        FIND Y.CONTRACT.NO IN Y.ALL.JOB.CONT.REFNO SETTING POS1,POS2,POS3 ELSE NULL
        REC.JOB.REG<BTB.JOB.CONT.STATUS,POS2> = 'CONTRACT REVERSED'
        GOSUB DECREMENT.JOB.REG
    END
RETURN

UPDATE.JOB.REG:
    REC.JOB.REG<BTB.JOB.CONT.ISSUE.DATE,POS2> = Y.SCT.ISS.DATE
    REC.JOB.REG<BTB.JOB.CONT.CURRENCY,POS2> = Y.SCT.CURRENCY
    REC.JOB.REG<BTB.JOB.CONT.AMOUNT,POS2> = Y.SCT.CONT.AMT
    REC.JOB.REG<BTB.JOB.CONT.NET.FOB.VALUE,POS2> = Y.NET.FOB.AMT
    REC.JOB.REG<BTB.JOB.CONT.SHIPMENT.DATE,POS2> = Y.SHIPMENT.DATE
    REC.JOB.REG<BTB.JOB.CONT.EXPIRY.DATE,POS2> = Y.EXPIRY.DATE
    REC.JOB.REG<BTB.JOB.CONT.BTB.ENTLMNT,POS2> = Y.CONT.BTB.ENT.AMT ;*update Current BTB Entitlement amount
    REC.JOB.REG<BTB.JOB.CONT.PC.ENTLMNT,POS2> = Y.CONT.PC.ENT.AMT ;*update Current BTB Entitlement amount
*
    REC.JOB.REG<BTB.JOB.TOT.BTB.ENT.AMT> += Y.BTB.ENT.AMT ;*update difference amount
    REC.JOB.REG<BTB.JOB.TOT.BTB.AVL.AMT> += Y.BTB.ENT.AMT ;*update difference amount
    REC.JOB.REG<BTB.JOB.TOT.PC.ENT.AMT> += Y.PC.ENT.AMT ;*update difference amount
    REC.JOB.REG<BTB.JOB.TOT.PC.AVL.AMT> += Y.PC.ENT.AMT ;*update difference amount
*
    REC.JOB.REG<BTB.JOB.TOT.EX.LC.AMT> += Y.CONV.SCT.CONT.AMT.DIFF
    REC.JOB.REG<BTB.JOB.TOT.NET.FOB.VALUE> += Y.CONV.NET.FOB.AMT.DIFF
*
    REC.JOB.REG<BTB.JOB.CO.CODE> = EB.SystemTables.getIdCompany()
    REC.JOB.REG<BTB.JOB.INPUTTER> = EB.SystemTables.getRNew(SCT.INPUTTER)
    REC.JOB.REG<BTB.JOB.AUTHORISER> = EB.SystemTables.getOperator()
    WRITE REC.JOB.REG ON F.BTB.JOB.REGISTER,Y.JOB.REG.ID
RETURN

DECREMENT.JOB.REG:
    REC.JOB.REG<BTB.JOB.CONT.ISSUE.DATE,POS2> = ''
    REC.JOB.REG<BTB.JOB.CONT.CURRENCY,POS2> = ''
    REC.JOB.REG<BTB.JOB.CONT.AMOUNT,POS2> -= Y.SCT.CONT.AMT
    REC.JOB.REG<BTB.JOB.CONT.NET.FOB.VALUE,POS2> -= Y.NET.FOB.AMT
    REC.JOB.REG<BTB.JOB.CONT.SHIPMENT.DATE,POS2> = ''
    REC.JOB.REG<BTB.JOB.CONT.EXPIRY.DATE,POS2> = ''
    REC.JOB.REG<BTB.JOB.CONT.BTB.ENTLMNT,POS2> -= Y.CONT.BTB.ENT.AMT ;*Substract Current Entitlement for Reverse record
    REC.JOB.REG<BTB.JOB.CONT.PC.ENTLMNT,POS2> -= Y.CONT.PC.ENT.AMT ;*Substract Current Entitlement for Reverse record
*
    REC.JOB.REG<BTB.JOB.TOT.BTB.ENT.AMT> -= Y.CONT.BTB.ENT.AMT ;*Substract Current Entitlement for Reverse record
    REC.JOB.REG<BTB.JOB.TOT.BTB.AVL.AMT> -= Y.CONT.BTB.ENT.AMT ;*Substract Current Entitlement for Reverse record
    REC.JOB.REG<BTB.JOB.TOT.PC.ENT.AMT> -= Y.CONT.PC.ENT.AMT ;*Substract Current Entitlement for Reverse record
    REC.JOB.REG<BTB.JOB.TOT.PC.AVL.AMT> -= Y.CONT.PC.ENT.AMT ;*Substract Current Entitlement for Reverse record
*
    REC.JOB.REG<BTB.JOB.TOT.EX.LC.AMT> -= Y.CONV.SCT.CONT.AMT
    REC.JOB.REG<BTB.JOB.TOT.NET.FOB.VALUE> -= Y.CONV.NET.FOB.AMT
*
    REC.JOB.REG<BTB.JOB.CO.CODE> = EB.SystemTables.getIdCompany()
    REC.JOB.REG<BTB.JOB.INPUTTER> = EB.SystemTables.getRNew(SCT.INPUTTER)
    REC.JOB.REG<BTB.JOB.AUTHORISER> = EB.SystemTables.getOperator()
*   WRITE REC.JOB.REG ON F.BTB.JOB.REGISTER,Y.JOB.REG.ID
    EB.DataAccess.FWrite(FN.BTB.JOB.REGISTER,Y.JOB.REG.ID,REC.JOB.REG)
    EB.TransactionControl.JournalUpdate('')
RETURN
END