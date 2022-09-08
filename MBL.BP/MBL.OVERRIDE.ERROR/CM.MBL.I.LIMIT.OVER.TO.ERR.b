SUBROUTINE CM.MBL.I.LIMIT.OVER.TO.ERR
*-----------------------------------------------------------------------------
*Subroutine Description:
*Subroutine Type:
*Attached To    :
*Attached As    :
*-----------------------------------------------------------------------------
* Modification History :
* /0/2020 -                            Create   - MD. EBRAHIM KHALIL RIAN,
*                                                 FDS Bangladesh Limited
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_GTS.COMMON
    $INSERT I_SOA.COMMON
    
    $USING LC.Contract
    $USING LC.Config
    
    $USING EB.DataAccess
    $USING EB.SystemTables
    $USING EB.ErrorProcessing
    $USING EB.OverrideProcessing
    $USING EB.LocalReferences
    
**********************************************
    WRITE.FILE.VAR = "OFS$OVERRIDES: ":OFS$OVERRIDES
    GOSUB FILE.WRITE
*****************************************************
*-----------------------------------------------------------------------------
    GOSUB INITIALISE ; *INITIALISATION
    GOSUB OPENFILE ; *FILE OPEN
    GOSUB PROCESS ; *PROCESS BUSINESS LOGIC
RETURN
*-----------------------------------------------------------------------------

*** <region name= INITIALISE>
INITIALISE:
*** <desc>INITIALISATION </desc>
    Y.APPLICATION = EB.SystemTables.getApplication()
    Y.VERSION = EB.SystemTables.getPgmVersion()
**********************************************
    WRITE.FILE.VAR = "Y.APPLICATION: ":Y.APPLICATION
    GOSUB FILE.WRITE
*****************************************************
**********************************************
    WRITE.FILE.VAR = "Y.VERSION: ":Y.VERSION
    GOSUB FILE.WRITE
*****************************************************
    
    FN.LETTER.OF.CREDIT = "F.LETTER.OF.CREDIT"
    F.LETTER.OF.CREDIT = ""
    
    FN.LC.TYPES = "F.LC.TYPES"
    F.LC.TYPES = ""
    
RETURN
*** </region>


*-----------------------------------------------------------------------------

*** <region name= OPENFILE>
OPENFILE:
*** <desc>FILE OPEN </desc>
    EB.DataAccess.Opf(FN.LETTER.OF.CREDIT, F.LETTER.OF.CREDIT)
    EB.DataAccess.Opf(FN.LC.TYPES, F.LC.TYPES)
RETURN
*** </region>

*** <region name= PROCESS>
PROCESS:
*** <desc>PROCESS BUSINESS LOGIC </desc>
    IF Y.APPLICATION EQ "DRAWINGS" THEN
        Y.DR.ID = EB.SystemTables.getIdNew()
        Y.LC.ID = Y.DR.ID[1,12]
        EB.DataAccess.FRead(FN.LETTER.OF.CREDIT, Y.LC.ID, R.LC, F.LETTER.OF.CREDIT, LC.ERR)
        IF R.LC THEN
            Y.LC.TYPE = R.LC<LC.Contract.LetterOfCredit.TfLcLcType>
            EB.DataAccess.FRead(FN.LC.TYPES, Y.LC.TYPE, R.LC.TYPES, FN.LC.TYPES, LC.TYPES.ERRR)
            Y.EX.IM = R.LC.TYPES<LC.Config.Types.TypImportExport>
            IF Y.EX.IM EQ "E" THEN RETURN
        END
    END
    
    IF Y.APPLICATION EQ "FUNDS.TRANSFER" AND Y.VERSION EQ ",MBL.AA.ACDI.EDF" THEN RETURN

    Y.SOA.OVERRIDES.COUNT = DCOUNT(SOA$OVERRIDES,'"')
    FOR I = 2 TO Y.SOA.OVERRIDES.COUNT STEP 2
        Y.OVERRIDE.ID = FIELD(SOA$OVERRIDES,'"',I)
        IF Y.OVERRIDE.ID EQ 'EXCESS.ID' THEN
            EB.SystemTables.setEtext("LI-EXCESS.ID")
            EB.ErrorProcessing.StoreEndError()
        END
    NEXT I

    Y.OVERRIDE.LIST = EB.SystemTables.getRNew(EB.SystemTables.getV()-9)
    Y.VM.COUNT = DCOUNT(Y.OVERRIDE.LIST, @VM)
**********************************************
    WRITE.FILE.VAR = "Y.VM.COUNT: ":Y.VM.COUNT
    GOSUB FILE.WRITE
**********************************************

    FOR I = 1 TO Y.VM.COUNT
        Y.ID.OVERRIDE = FIELD(Y.OVERRIDE.LIST<1,I>, "}", 1)
**********************************************
        WRITE.FILE.VAR = "Y.ID.OVERRIDE: ":Y.ID.OVERRIDE
        GOSUB FILE.WRITE
**********************************************
        IF Y.ID.OVERRIDE EQ "EXCESS.ID" THEN
            EB.SystemTables.setEtext("LI-EXCESS.ID")
            EB.ErrorProcessing.StoreEndError()
        END
   
    NEXT I
RETURN
*** </region>

*****************************************************
FILE.WRITE:
*    Y.LOG.FILE='CM.MBL.I.LIMIT.OVER.TO.ERR.txt'
*    Y.FILE.DIR ='./DFE.TEST'
*    OPENSEQ Y.FILE.DIR,Y.LOG.FILE TO F.FILE.DIR ELSE NULL
*    WRITESEQ WRITE.FILE.VAR APPEND TO F.FILE.DIR ELSE NULL
*    CLOSESEQ F.FILE.DIR
RETURN
********************************************************
END
