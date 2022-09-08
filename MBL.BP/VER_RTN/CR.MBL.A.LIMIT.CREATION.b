SUBROUTINE CR.MBL.A.LIMIT.CREATION
*-----------------------------------------------------------------------------
*Subroutine Description:
*Subroutine Type:
*Attached To    : LIMIT,MBL.SECURED Version
*Attached As    : AUTH ROUTINE
*-----------------------------------------------------------------------------
* Modification History :
* 08/03/2020 -                            Retrofit   - MD. EBRAHIM KHALIL RIAN,
*                                                 FDS Bangladesh Limited
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
*
    $USING LI.Config
    $USING EB.DataAccess
    $USING EB.SystemTables
    $USING EB.LocalReferences
*-----------------------------------------------------------------------------
    GOSUB INITIALISE ; *INITIALISATION
    GOSUB OPENFILE ; *FILE OPEN
    GOSUB PROCESS ; *PROCESS BUSINESS LOGIC
RETURN
*-----------------------------------------------------------------------------
*
*** <region name= INITIALISE>
INITIALISE:
*** <desc>INITIALISATION </desc>
*    EB.LocalReferences.GetLocRef("LIMIT","LT.LM.EXCL.SLVL",Y.SUB.LVL.POS)
*    Y.SUB.LVL.VALUE = EB.SystemTables.getRNew(LI.Config.Limit.LocalRef)<1,Y.SUB.LVL.POS>
*    IF Y.SUB.LVL.VALUE NE 'Y' THEN RETURN
*
    FN.LIMIT.NAU = "FBNK.LIMIT$NAU"
    F.LIMIT.NAU = ""
*
    REC.SUB.LIMIT=""
    REC.CHILD.LIMIT=""
*
    SUB.PARENT="LT.LM.SUB.PARNT"
    SUB.PARENT.POS=""
    EB.LocalReferences.GetLocRef("LIMIT",SUB.PARENT,SUB.PARENT.POS)
*
    CHILD.LIMIT="LT.LM.CHILD.LMT"
    CHILD.LIMIT.POS=""
    EB.LocalReferences.GetLocRef("LIMIT",CHILD.LIMIT,CHILD.LIMIT.POS)
RETURN
*** </region>
*
*-----------------------------------------------------------------------------
*
*** <region name= OPENFILE>
OPENFILE:
*** <desc>FILE OPEN </desc>
    EB.DataAccess.Opf(FN.LIMIT.NAU,F.LIMIT.NAU)
RETURN
*** </region>
*
*** <region name= PROCESS>
PROCESS:
*** <desc>PROCESS BUSINESS LOGIC </desc>
    IF SUB.PARENT.POS THEN
        Y.SUB.PR.ID=""
        IF FIELD(EB.SystemTables.getIdNew(),".",4,1) NE '' THEN
            Y.SUB.PR.ID=FIELD(EB.SystemTables.getIdNew(),".",1,1):".":FMT(EB.SystemTables.getRNew(LI.Config.Limit.LocalRef)<1,SUB.PARENT.POS>,"7'0'R"):".":FIELD(EB.SystemTables.getIdNew(),".",3,1):".":FIELD(EB.SystemTables.getIdNew(),".",4,1)
        END
        ELSE
            Y.SUB.PR.ID=FIELD(EB.SystemTables.getIdNew(),".",1,1):".":FMT(EB.SystemTables.getRNew(LI.Config.Limit.LocalRef)<1,SUB.PARENT.POS>,"7'0'R"):".":FIELD(EB.SystemTables.getIdNew(),".",3,1)
        END
        Y.SUB.VAL=EB.SystemTables.getRNew(LI.Config.Limit.LocalRef)<1,SUB.PARENT.POS>
        IF Y.SUB.VAL NE '' THEN
            FOR I=1 TO 500
                BEGIN CASE
                    CASE I EQ 96
                        REC.SUB.LIMIT<I>=EB.SystemTables.getIdNew()
                    CASE I EQ 100
                        REC.SUB.LIMIT<I>=EB.SystemTables.getRNew(LI.Config.Limit.LocalRef)<1,SUB.PARENT.POS>
                    CASE 1
                        REC.SUB.LIMIT<I>=EB.SystemTables.getRNew(I)
                END CASE
            NEXT
*
            WRITE REC.SUB.LIMIT TO F.LIMIT.NAU,Y.SUB.PR.ID
        END
    END
*
    IF CHILD.LIMIT.POS THEN
        Y.CHLD.ID=""
        IF FIELD(EB.SystemTables.getIdNew(),".",4,1) NE '' THEN
            Y.CHLD.ID=FIELD(EB.SystemTables.getIdNew(),".",1,1):".":FMT(EB.SystemTables.getRNew(LI.Config.Limit.LocalRef)<1,CHILD.LIMIT.POS>,"7'0'R"):".":FIELD(EB.SystemTables.getIdNew(),".",3,1):".":FIELD(EB.SystemTables.getIdNew(),".",4,1)
        END ELSE
            Y.CHLD.ID=FIELD(EB.SystemTables.getIdNew(),".",1,1):".":FMT(EB.SystemTables.getRNew(LI.Config.Limit.LocalRef)<1,CHILD.LIMIT.POS>,"7'0'R"):".":FIELD(EB.SystemTables.getIdNew(),".",3,1)
        END
        Y.CHILD.VAL = EB.SystemTables.getRNew(LI.Config.Limit.LocalRef)<1,CHILD.LIMIT.POS>
        IF Y.CHILD.VAL NE '' THEN
            FOR J=1 TO 500
                BEGIN CASE
                    CASE J EQ 96
                        REC.CHILD.LIMIT<J>=Y.SUB.PR.ID
                    CASE J EQ 100
                        REC.CHILD.LIMIT<J>=EB.SystemTables.getRNew(LI.Config.Limit.LocalRef)<1,CHILD.LIMIT.POS>
                    CASE 1
                        REC.CHILD.LIMIT<J>=EB.SystemTables.getRNew(J)
                END CASE
            NEXT
*
            WRITE REC.CHILD.LIMIT TO F.LIMIT.NAU,Y.CHLD.ID
        END
    END
*
RETURN
*** </region>
END