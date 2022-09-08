* @ValidationCode : MjotMTE2NjE1NzgwNTpDcDEyNTI6MTU2MjAxODcyNDcwNjpERUxMOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjE3X0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 02 Jul 2019 04:05:24
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : DELL
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R17_AMR.0
SUBROUTINE TF.MBL.A.BTB.UPDT.SCT.TO.JOB.REG
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.BD.SCT.CAPTURE
    $INSERT I_F.BD.BTB.CUSTOMER.SEQ.NO
    $INSERT I_F.BD.BTB.JOB.REGISTER
    $INSERT I_F.BD.LC.AD.CODE
*-----------------------------------------------------------------------------
    $USING EB.SystemTables
    $USING EB.DataAccess
    $USING EB.Foundation
    $USING EB.ErrorProcessing
    $USING ST.CompanyCreation
    $USING EB.TransactionControl
    
    GOSUB INIT
    GOSUB OPENFILES
    GOSUB PROCESS
RETURN

INIT:
    FN.BTB.JOB.REGISTER = 'F.BD.BTB.JOB.REGISTER'
    F.BTB.JOB.REGISTER = ''
    
    FN.BD.BTB.JOB.CUS.SEQNO = 'F.BD.BTB.CUSTOMER.SEQ.NO'
    F.BD.BTB.JOB.CUS.SEQNO = ''
    
    FN.BD.LC.AD.CODE= 'F.BD.LC.AD.CODE'
    F.BD.LC.AD.CODE = ''
    
    Y.COMPANY.ID = EB.SystemTables.getIdCompany()
    CUS.ID = EREPLACE(FIELD(EB.SystemTables.getIdNew(),'.',1,1),'SCT','')
RETURN

OPENFILES:
    EB.DataAccess.Opf(FN.BTB.JOB.REGISTER,F.BTB.JOB.REGISTER)
    EB.DataAccess.Opf(FN.BD.BTB.JOB.CUS.SEQNO,F.BD.BTB.JOB.CUS.SEQNO)
    EB.DataAccess.Opf(FN.BD.LC.AD.CODE,F.BD.LC.AD.CODE)
RETURN

PROCESS:
    Y.NEW.EXIST = EB.SystemTables.getRNew(SCT.NEW.EXIST.JOB.NO)
    Y.SCT.CURRENCY = EB.SystemTables.getRNew(SCT.CURRENCY)
    Y.JOB.CURRENCY = EB.SystemTables.getRNew(SCT.JOB.CURRENCY)
    Y.SCT.ISS.DATE = EB.SystemTables.getRNew(SCT.CONTRACT.DATE)
    Y.JOB.EXC.RATE = EB.SystemTables.getRNew(SCT.JOB.EXCHG.RATE)
    Y.SCT.CONT.AMT = EB.SystemTables.getRNew(SCT.CONTRACT.AMT)
    Y.CONV.SCT.CONT.AMT = EB.SystemTables.getRNew(SCT.CONTRACT.AMT) * Y.JOB.EXC.RATE
    Y.SHIPMENT.DATE = EB.SystemTables.getRNew(SCT.SHIPMENT.DATE)
    Y.EXPIRY.DATE = EB.SystemTables.getRNew(SCT.EXPIRY.DATE)
    Y.NET.FOB.AMT = EB.SystemTables.getRNew(SCT.NET.FOB.VALUE)
    Y.CONV.NET.FOB.AMT = EB.SystemTables.getRNew(SCT.NET.FOB.VALUE) * Y.JOB.EXC.RATE
*   Y.BTB.ENT.AMT = DROUND((Y.JOB.EXC.RATE * EB.SystemTables.getRNew(SCT.BTB.ENT.AMT)),2)
*   Y.PC.ENT.AMT = DROUND((Y.JOB.EXC.RATE * EB.SystemTables.getRNew(SCT.PCECC.ENT.AMT)),2)
    Y.CONT.BTB.ENT.AMT = EB.SystemTables.getRNew(SCT.BTB.ENT.AMT)
    Y.CONT.PC.ENT.AMT = EB.SystemTables.getRNew(SCT.PCECC.ENT.AMT)
    
    BEGIN CASE
        CASE Y.NEW.EXIST EQ 'NEW'
            GOSUB GENERATE.JOB.REG.ID
            EB.DataAccess.FRead(FN.BTB.JOB.REGISTER,Y.JOB.REG.ID, REC.JOB.REG, F.BTB.JOB.REGISTER, ERR.JOB.SEQREG)
            REC.JOB.REG<BTB.JOB.CUSTOMER.NO> = CUS.ID
            REC.JOB.REG<BTB.JOB.JOB.CREATE.DATE> = EB.SystemTables.getToday()
            REC.JOB.REG<BTB.JOB.JOB.CURRENCY> = EB.SystemTables.getRNew(SCT.JOB.CURRENCY)
            REC.JOB.REG<BTB.JOB.JOB.EXPIRY.DATE> = Y.EXPIRY.DATE
            Y.COUNT = "1"
            GOSUB UPDATE.JOB.REGISTER
            EB.SystemTables.setRNew(SCT.BTB.JOB.NO, Y.JOB.REG.ID)
        CASE Y.NEW.EXIST = 'EXIST'
            Y.JOB.REG.ID = EB.SystemTables.getRNew(SCT.BTB.JOB.NO)
            IF Y.JOB.REG.ID EQ '' THEN
                EB.SystemTables.setEtext('JOB NUMBER MISSING')
                EB.ErrorProcessing.StoreEndError()
                RETURN
            END
            EB.DataAccess.FRead(FN.BTB.JOB.REGISTER,Y.JOB.REG.ID, REC.JOB.REG, F.BTB.JOB.REGISTER, ERR.JOB.SEQREG)
            IF REC.JOB.REG EQ '' THEN
                EB.SystemTables.setEtext('JOB NUMBER DOES NOT EXIST')
                EB.ErrorProcessing.StoreEndError()
                RETURN
            END
            Y.COUNT = DCOUNT(REC.JOB.REG<BTB.JOB.CONT.REFNO>,VM) + 1
            GOSUB UPDATE.JOB.REGISTER
    END CASE
RETURN

GENERATE.JOB.REG.ID:
* For new job generating job register id and update the sequence number of template BD.L.BTB.CUS.JOB.SEQNO
    EB.DataAccess.FRead(FN.BD.LC.AD.CODE,EB.SystemTables.getIdCompany(),R.BD.LC.AD.CODE,F.BD.LC.AD.CODE,BD.LC.AD.CODE.ERR)
    IF BD.LC.AD.CODE.ERR THEN
        EB.SystemTables.setE("AD CODE DOES NOT EXIST FOR THIS COMPANY")
        EB.ErrorProcessing.StoreEndError()
        RETURN
    END ELSE
        Y.AD.BR.CODE = R.BD.LC.AD.CODE<AD.CODE.AD.CODE>
    END
    
    EB.DataAccess.FRead(FN.BD.BTB.JOB.CUS.SEQNO,CUS.ID,R.BD.BTB.JOB.CUS.SEQNO,F.BD.BTB.JOB.CUS.SEQNO,Y.CUST.SEQ.ERR)
    IF R.BD.BTB.JOB.CUS.SEQNO THEN
        Y.CUST.SEQNO = R.BD.BTB.JOB.CUS.SEQNO<BD.LC.SEQ.NO> + 1
    END ELSE
        Y.CUST.SEQNO = "1"
    END
    R.BD.BTB.JOB.CUS.SEQNO<BD.LC.SEQ.NO> = Y.CUST.SEQNO
    WRITE R.BD.BTB.JOB.CUS.SEQNO TO F.BD.BTB.JOB.CUS.SEQNO,CUS.ID
    
    Y.JOB.REG.SEQNO.ID = Y.AD.BR.CODE : '.' : CUS.ID
    Y.TODAY = EB.SystemTables.getToday()
    ZEROS = '00000'
    Y.JOB.REG.SEQNO = ZEROS[1,LEN(ZEROS)-LEN(Y.CUST.SEQNO)]:Y.CUST.SEQNO
    Y.JOB.REG.ID = Y.JOB.REG.SEQNO.ID : '.' : Y.JOB.REG.SEQNO:'.':Y.TODAY[3,2]
RETURN

UPDATE.JOB.REGISTER:
    REC.JOB.REG<BTB.JOB.CONT.REFNO,Y.COUNT> = EB.SystemTables.getIdNew()
    REC.JOB.REG<BTB.JOB.CONT.ISSUE.DATE,Y.COUNT> = Y.SCT.ISS.DATE
    REC.JOB.REG<BTB.JOB.CONT.CURRENCY,Y.COUNT> = Y.SCT.CURRENCY
    REC.JOB.REG<BTB.JOB.CONT.AMOUNT,Y.COUNT> = Y.SCT.CONT.AMT
    REC.JOB.REG<BTB.JOB.CONT.NET.FOB.VALUE,Y.COUNT> = Y.NET.FOB.AMT
    REC.JOB.REG<BTB.JOB.CONT.SHIPMENT.DATE,Y.COUNT> = Y.SHIPMENT.DATE
    REC.JOB.REG<BTB.JOB.CONT.EXPIRY.DATE,Y.COUNT> = Y.EXPIRY.DATE
    REC.JOB.REG<BTB.JOB.CONT.BTB.ENTLMNT,Y.COUNT> = Y.CONT.BTB.ENT.AMT
    REC.JOB.REG<BTB.JOB.CONT.PC.ENTLMNT,Y.COUNT> = Y.CONT.PC.ENT.AMT
*
    REC.JOB.REG<BTB.JOB.TOT.PC.ENT.AMT> += Y.CONT.PC.ENT.AMT
    REC.JOB.REG<BTB.JOB.TOT.PC.AVL.AMT> += Y.CONT.PC.ENT.AMT
    Y.BTB.JOB.TOT.PC.ENT.AMT = REC.JOB.REG<BTB.JOB.TOT.PC.ENT.AMT>
    Y.BTB.JOB.TOT.PC.AVL.AMT = REC.JOB.REG<BTB.JOB.TOT.PC.AVL.AMT>
    REC.JOB.REG<BTB.JOB.TOT.PC.AMT> = Y.BTB.JOB.TOT.PC.ENT.AMT - Y.BTB.JOB.TOT.PC.AVL.AMT
*
    REC.JOB.REG<BTB.JOB.TOT.BTB.ENT.AMT> += Y.CONT.BTB.ENT.AMT
    REC.JOB.REG<BTB.JOB.TOT.BTB.AVL.AMT> += Y.CONT.BTB.ENT.AMT
    Y.BTB.JOB.TOT.BTB.ENT.AMT = REC.JOB.REG<BTB.JOB.TOT.BTB.ENT.AMT>
    Y.BTB.JOB.TOT.BTB.AVL.AMT = REC.JOB.REG<BTB.JOB.TOT.BTB.AVL.AMT>
    REC.JOB.REG<BTB.JOB.TOT.BTB.AMT> = Y.BTB.JOB.TOT.BTB.ENT.AMT - Y.BTB.JOB.TOT.BTB.AVL.AMT
*
    REC.JOB.REG<BTB.JOB.TOT.EX.LC.AMT> += Y.CONV.SCT.CONT.AMT
    REC.JOB.REG<BTB.JOB.TOT.NET.FOB.VALUE> += Y.CONV.NET.FOB.AMT
*
    REC.JOB.REG<BTB.JOB.CO.CODE> = EB.SystemTables.getIdCompany()
    REC.JOB.REG<BTB.JOB.INPUTTER> = EB.SystemTables.getRNew(SCT.INPUTTER)
    REC.JOB.REG<BTB.JOB.AUTHORISER> = EB.SystemTables.getOperator()
*   WRITE REC.JOB.REG ON F.BTB.JOB.REGISTER,Y.JOB.REG.ID
    EB.DataAccess.FWrite(FN.BTB.JOB.REGISTER,Y.JOB.REG.ID,REC.JOB.REG)
    EB.TransactionControl.JournalUpdate('')
RETURN
END