* @ValidationCode : MjotMTc5Mjg0MjAyODpDcDEyNTI6MTU3NDI2NDU4OTU1NzpERUxMOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjE3X0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 20 Nov 2019 21:43:09
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : DELL
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R17_AMR.0
SUBROUTINE TF.MBL.A.OUTCOL.DR.UPDT.TO.JOB
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
*-----------------------------------------------------------------------------
    $INSERT I_F.BD.BTB.JOB.REGISTER
*-----------------------------------------------------------------------------
    $USING EB.SystemTables
    $USING EB.DataAccess
    $USING EB.ErrorProcessing
    $USING EB.OverrideProcessing
    $USING LC.Contract
    $USING EB.Updates
    $USING EB.TransactionControl
*-----------------------------------------------------------------------------
    GOSUB INIT
    GOSUB OPENFILES
    GOSUB PROCESS
RETURN

*****
INIT:
*****
    FN.JOB.REG = 'F.BD.BTB.JOB.REGISTER'
    F.JOB.REG = ''
    FN.LC = 'F.LETTER.OF.CREDIT'
    F.LC = ''

    FLD.POS = ''
    APPLICATION.NAME = 'LETTER.OF.CREDIT'
    LOCAL.FIELD = 'LT.TF.SCONT.ID':VM:'LT.TF.JOB.NUMBR'
    EB.Updates.MultiGetLocRef(APPLICATION.NAME,LOCAL.FIELD,FLD.POS)
    Y.SCONT.ID.POS = FLD.POS<1,1>
    Y.JOB.NUMBER.POS = FLD.POS<1,2>

    Y.DR.ID = EB.SystemTables.getIdNew()
    Y.LC.ID = Y.DR.ID[1, LEN(Y.DR.ID)-2]
RETURN

***********
OPENFILES:
***********
    EB.DataAccess.Opf(FN.JOB.REG,F.JOB.REG)
    EB.DataAccess.Opf(FN.LC,F.LC)
RETURN

********
PROCESS:
********
    EB.DataAccess.FRead(FN.LC,Y.LC.ID,R.LC.REC,F.LC,LC.ERR)
    Y.SCONT.ID = R.LC.REC<LC.Contract.LetterOfCredit.TfLcLocalRef,Y.SCONT.ID.POS,1>
    Y.JOB.REG.ID = R.LC.REC<LC.Contract.LetterOfCredit.TfLcLocalRef,Y.JOB.NUMBER.POS,1>
    EB.DataAccess.FRead(FN.JOB.REG,Y.JOB.REG.ID,R.JOB.REG,F.JOB.REG,E.JOB.REG)
    Y.JOB.SCONT.LST = R.JOB.REG<BTB.JOB.CONT.REFNO>
    FIND Y.SCONT.ID IN Y.JOB.SCONT.LST SETTING Y.JOB.SCT.REF.POS1, Y.JOB.SCT.REF.POS2, Y.JOB.SCT.REF.POS3 THEN
        Y.JOB.COLL.TF.REFNO.LST = R.JOB.REG<BTB.JOB.COLL.TF.REFNO, Y.JOB.SCT.REF.POS2>
        FIND Y.LC.ID IN Y.JOB.COLL.TF.REFNO.LST SETTING Y.JOB.COLL.TF.POS1, Y.JOB.COLL.TF.POS2, Y.JOB.COLL.TF.POS3  ELSE NULL
        IF Y.JOB.COLL.TF.POS3 THEN
            R.JOB.REG<BTB.JOB.COLL.DR.REFNO, Y.JOB.SCT.REF.POS2, Y.JOB.COLL.TF.POS3> = Y.DR.ID
*           WRITE R.JOB.REG ON F.JOB.REG,Y.JOB.REG.ID
            EB.DataAccess.FWrite(FN.JOB.REG,Y.JOB.REG.ID,R.JOB.REG)
            EB.TransactionControl.JournalUpdate('')
        END
    END
RETURN

END
