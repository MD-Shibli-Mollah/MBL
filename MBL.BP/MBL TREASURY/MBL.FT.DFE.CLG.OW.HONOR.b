* @ValidationCode : MjotMjI5Mjc1MTc1OkNwMTI1MjoxNjM0Nzk1MjkzNTM4OnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 21 Oct 2021 11:48:13
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE MBL.FT.DFE.CLG.OW.HONOR(INCOMING.ARG)

*-----------------------------------------------------------------------------
* Description   : This is the post routine attached to DFE.MAPPING
* Type          : DFE.MAPPING>POST.ROUTINE
* DFE.MAPPING   : MBL.DFE.FT.CHQ.CLG.OW.HONOR,MBL.DFE.FT.CHQ.CLG.OW.RETURN,MBL.DFE.FT.EFT.IW,MBL.DFE.FT.EFT.IW.RETURN
* Linked With   : DFE.MAPPING
* In Parameter  : INCOMING.ARG
* Out Parameter : INCOMING.ARG
*-----------------------------------------------------------------------------
* Revision History:
*------------------
* Date         - 12/05/2021
* Done By      - MD SHIBLI MOLLAH

* Modification History:
*------------------
* Date         - 27/09/2021
* Done By      - MD SHIBLI MOLLAH
*   Success Data******* in 113

**********************************************
*** <region name= Inserts>
    $INSERT I_DFE.FRAME.WORK.COMMON
    $INSERT I_F.DFE.OUTWARD.WORK.FILE
    $INSERT I_DFE.OUTWARD.FILE.EXTRACT.COMMON
    
    $USING FT.Contract
    $USING EB.SystemTables
    $USING EB.Utility
    $USING EB.Interface
    $USING EB.DataAccess

*** </region>
*-----------------------------------------------------------------------------
*** <region name= Main Control Logic>
    GOSUB INITIALISE
    GOSUB PROCESS

RETURN
*** </region>

*-----------------------------------------------------------------------------
*** <region name= Initialise>
*** <desc>Common variables are initialised</desc>
INITIALISE:
    TODAY = EB.SystemTables.getToday()
    MAPPING.ID = EB.SystemTables.getBatchDetails()<3> ; Y.DFE.ID = FIELD(INCOMING.ARG,"|",1)
    Y.OFS.ID = FIELD(Y.DFE.ID,"/",2)
    Y.ERR.CHK = FIELD(Y.DFE.ID,"/",3)
    
    FN.FT = 'F.FUNDS.TRANSFER'
    F.FT = ''
    
    FN.OFS.REQUEST.DETAIL = 'F.OFS.REQUEST.DETAIL'
    F.OFS.REQUEST.DETAIL = ''
    
*    EB.DataAccess.Opf(FN.FT, F.FT)

RETURN

*** </region>
*----------------------------------------------------------------------------------
*** <region name= PROCESS>
***<desc>Based on mapping id, Update batch or entries details</desc>
*-------
PROCESS:
*--------
*---------------------------------------------WRITE DATA-----------------------------------------------
    IF Y.ERR.CHK EQ '-1' THEN
        WriteData = 'CURRENT.RECORD: ':C$CURRENT.RECORD:
        FileName = MAPPING.ID:'.':TODAY:'.Err.txt'
        FilePath = '/t24/mbllive/T24/UD/DFE/ERROR.FILES'
        OPENSEQ FilePath,FileName TO FileOutput THEN NULL
        ELSE
            CREATE FileOutput ELSE
            END
        END
        WRITESEQ WriteData APPEND TO FileOutput ELSE
            CLOSESEQ FileOutput
        END
        CLOSESEQ FileOutput
    END
    IF Y.ERR.CHK EQ '-1' THEN
        WriteData = 'DATA WITH ERROR: ':INCOMING.ARG
        FileName = MAPPING.ID:'.':TODAY:'.Err.txt'
        FilePath = '/t24/mbllive/T24/UD/DFE/ERROR.FILES'
        OPENSEQ FilePath,FileName TO FileOutput THEN NULL
        ELSE
            CREATE FileOutput ELSE
            END
        END
        WRITESEQ WriteData APPEND TO FileOutput ELSE
            CLOSESEQ FileOutput
        END
        CLOSESEQ FileOutput
    END
    
******Success Data***************27 SEP 2021-------------------------------------

    IF Y.ERR.CHK EQ '1' THEN
        WriteData = 'CURRENT.RECORD: ':C$CURRENT.RECORD:
        FileName = MAPPING.ID:'.':TODAY:'.Success.txt'
        FilePath = '/t24/mbllive/T24/UD/DFE/ERROR.FILES'
        OPENSEQ FilePath,FileName TO FileOutput THEN NULL
        ELSE
            CREATE FileOutput ELSE
            END
        END
        WRITESEQ WriteData APPEND TO FileOutput ELSE
            CLOSESEQ FileOutput
        END
        CLOSESEQ FileOutput
    END
*    IF Y.ERR.CHK EQ '1' THEN
*        WriteData = 'DATA WITH Success: ':INCOMING.ARG
*        FileName = MAPPING.ID:'.':TODAY:'.Success.txt'
*        FilePath = '/t24/mbllive/T24/UD/DFE/ERROR.FILES'
*        OPENSEQ FilePath,FileName TO FileOutput THEN NULL
*        ELSE
*            CREATE FileOutput ELSE
*            END
*        END
*        WRITESEQ WriteData APPEND TO FileOutput ELSE
*            CLOSESEQ FileOutput
*        END
*        CLOSESEQ FileOutput
*    END
*--------------------------------------------------------------------------------------------
RETURN
END