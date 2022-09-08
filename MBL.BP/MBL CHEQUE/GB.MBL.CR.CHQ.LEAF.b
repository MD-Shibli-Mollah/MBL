* @ValidationCode : MjoxNjM0MzM3MDA0OkNwMTI1MjoxNjEyODUwMzM0OTc1OnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 09 Feb 2021 11:58:54
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0
SUBROUTINE GB.MBL.CR.CHQ.LEAF
*-----------------------------------------------------------------------------
*Subroutine Description:
* This routine is use for Cheque Number, Cheque no not zero or blank validation
*Subroutine Type:
*Attached To    : version(CHEQUE.ISSUE,MBL.INPUT)
*Attached As    : CHECK RECORD ROUTINE
*-----------------------------------------------------------------------------
* Modification History :

* DEVELOPED BY                                MD SHIBLI MOLLAH
* ON 08/02/2021                                   FDS Bangladesh Limited

*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $USING ST.ChqIssue
    $USING ST.ChqSubmit
    $USING EB.DataAccess
    $USING EB.SystemTables
    $USING EB.OverrideProcessing
    $USING EB.Foundation
    $USING EB.ErrorProcessing
*-----------------------------------------------------------------------------
*    GOSUB INITIALISE ; *INITIALISATION
*    GOSUB OPENFILE ; *FILE OPEN
    GOSUB PROCESS ; *PROCESS BUSINESS LOGIC
RETURN

PROCESS:
*** <desc>PROCESS BUSINESS LOGIC </desc>
***--------------*-----------------------
    Y.CHQ.COM.CODE.POS=""
    Y.CHQ.NO.START.POS=""
    Y.APP.NAME = "CHEQUE.ISSUE"
    LOCAL.FIELDS = ""
    LOCAL.FIELDS = "LT.CHQ.LEAF.NO"
    FLD.POS = ""
    EB.Foundation.MapLocalFields(Y.APP.NAME, LOCAL.FIELDS,FLD.POS)
    Y.CHQ.LEAF.NO.POS = FLD.POS<1,1>
*--------------*-------------------------

    Y.CHQ.NM.COMI = EB.SystemTables.getComi()
    Y.CHQ.LEAF.NO = EB.SystemTables.getRNew(ST.ChqIssue.ChequeIssue.ChequeIsLocalRef)
    Y.CHQ.LEAF = Y.CHQ.LEAF.NO<1,Y.CHQ.LEAF.NO.POS>
 
    EB.SystemTables.setRNew(ST.ChqIssue.ChequeIssue.ChequeIsNumberIssued, Y.CHQ.LEAF)
* MYTEST = EB.SystemTables.setRNew(ST.ChqIssue.ChequeIssue.ChequeIsNumberIssued, Y.CHQ.LEAF)

*<-> TRACER
*    WriteData = Y.CHQ.LEAF.NO:" ":MYTEST:" ":Y.CHQ.LEAF:" ": Y.CHQ.LEAF.NO:" ":Y.CHQ.NM.COMI
*    FileName = 'SHIBLI_CHEQUE.ISSUE.LEAF.txt'
*    FilePath = 'DL.BP'
*    OPENSEQ FilePath,FileName TO FileOutput THEN NULL
*    ELSE
*        CREATE FileOutput ELSE
*        END
*    END
*    WRITESEQ WriteData APPEND TO FileOutput ELSE
*        CLOSESEQ FileOutput
*    END
*    CLOSESEQ FileOutput
*<>
RETURN
*** </region>
END