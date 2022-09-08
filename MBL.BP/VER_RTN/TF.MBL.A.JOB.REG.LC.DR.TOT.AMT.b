SUBROUTINE TF.MBL.A.JOB.REG.LC.DR.TOT.AMT
*-----------------------------------------------------------------------------
*Attached As    : THIS ROUTINE WILL ATTACHED TO ALL EXPORT LC & DR(COLLECTION) AND SALES CONTACT & DR.
*                 it will update job register TOT.EX.LC.AMT, TOT.NET.FOB.VALUE, TOT.EX.LC.DRAW.AMT
*                 EX.LC.SHIP.DATE, TOT.PC.ENT.AMT, TOT.BTB.ENT.AMT fields.
*-----------------------------------------------------------------------------
* Modification History :
* 10/8/2020 -                            Creator   - MAHMUDUR RAHMAN (UDOY),
*                                                 FDS Bangladesh Limited
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.BD.BTB.JOB.REGISTER
    $INSERT I_F.BD.SCT.CAPTURE
    
    $USING LC.Contract
    $USING EB.SystemTables
    $USING EB.DataAccess
    $USING EB.LocalReferences


*-----------------------------------------------------------------------------

    IF EB.SystemTables.getVFunction() EQ 'A' THEN
        GOSUB INITIALISE ; *INITIALISATION
        GOSUB OPENFILE ; *FILE OPEN
        GOSUB PROCESS ; *PROCESS BUSINESS LOGIC
    END
    
RETURN
*-----------------------------------------------------------------------------

INITIALISE:
 
    FN.JOB='F.BD.BTB.JOB.REGISTER' 
    F.JOB =''
    
    FN.LC = 'F.LETTER.OF.CREDIT'
    F.LC  = ''
    
    FN.DR = 'F.DRAWINGS'
    F.DR  = ''

    EB.LocalReferences.GetLocRef("LETTER.OF.CREDIT","LT.TF.JOB.NUMBR",LT.TF.JOB.NUMBER.POS)
    
    Y.APP.ID = EB.SystemTables.getIdNew()
    Y.APP = EB.SystemTables.getApplication()
    
    
    BEGIN CASE
        CASE Y.APP EQ 'DRAWINGS'
            Y.LC.ID   = Y.APP.ID[1,LEN(Y.APP.ID)-2]
            EB.DataAccess.FRead(FN.LC, Y.LC.ID, LC.REC, F.LC, LC.ERR)
            Y.LC.REF = LC.REC<LC.Contract.LetterOfCredit.TfLcLocalRef>
            Y.JOB.NUM = Y.LC.REF<1,LT.TF.JOB.NUMBER.POS>
        CASE Y.APP EQ 'LETTER.OF.CREDIT'
            Y.JOB.NUM = EB.SystemTables.getRNew(LC.Contract.LetterOfCredit.TfLcLocalRef)<1,LT.TF.JOB.NUMBER.POS>
            Y.LATEST.SHIP.DATE = EB.SystemTables.getRNew(LC.Contract.LetterOfCredit.TfLcLatestShipment)
            Y.LC.ID   = Y.APP.ID
        CASE Y.APP EQ 'BD.SCT.CAPTURE'
            Y.JOB.NUM = EB.SystemTables.getRNew(SCT.BTB.JOB.NO)
    END CASE
   
RETURN


OPENFILE:

    EB.DataAccess.Opf(FN.JOB,F.JOB)
    EB.DataAccess.Opf(FN.DR,F.DR)
RETURN

PROCESS:
 ************** SALES CONTACT PART CALCULATION FILED*********************
    Y.TOT.CON.AMOUNT = ''
    Y.TOT.CON.FOB.VALUE = ''
    Y.TOT.PC.VALUE = ''
    Y.TOT.BTB.VALUE = ''
    Y.TOT.CON.DR.AMT = ''
 ***********************************    
    EB.DataAccess.FRead(FN.JOB, Y.JOB.NUM, JOB.REC, F.JOB, ERR.REC)
    GOSUB SALE.CONTACT ;*
    Y.LC.AMOUNT     = JOB.REC<BTB.JOB.EX.LC.AMOUNT> ;*ASOSIATE MULTIVALUE FIELD
    Y.LC.AMT.SUM    = SUM(Y.LC.AMOUNT)
    Y.TOT.LC.AMOUNT = DROUND(Y.LC.AMT.SUM,2)
    Y.FOB.VALUE     = JOB.REC<BTB.JOB.EX.NET.FOB.VALUE> ;*ASOSIATE MULTIVALUE FIELD
    Y.TOT.FOB.SUM   = SUM(Y.FOB.VALUE)
    Y.TOT.FOB.VALUE = DROUND(Y.TOT.FOB.SUM,2)
    Y.DR.AMT        = JOB.REC<BTB.JOB.EX.DR.AMT> ;*ASOSIATE MULTIVALUE FIELD
    
    CONVERT SM TO VM IN Y.DR.AMT
     
    Y.TOT.DR.SUM    = SUM(Y.DR.AMT)
    Y.TOT.DR.AMT    = DROUND(Y.TOT.DR.SUM,2)
    Y.TOT.JOB.LC    = JOB.REC<BTB.JOB.EX.TF.REF> ;*ASOSIATE MULTIVALUE FIELD
    
 
    
    Y.LC.COUNT = DCOUNT(Y.TOT.JOB.LC, @VM)
    
    FOR I = 1 TO Y.LC.COUNT
        Y.LC.NUM = Y.TOT.JOB.LC<1,I>
        IF Y.LC.NUM EQ Y.LC.ID THEN
            Y.LC.SHIP.DATE.POS = I
            BREAK
        END
    NEXT I
    
*****************MARGE SALES & EX LC DATA FROM JOB******************************
    Y.TOT.LC.AMOUNT += Y.TOT.CON.AMOUNT
    Y.TOT.FOB.VALUE += Y.TOT.CON.FOB.VALUE
    Y.TOT.DR.AMT    += Y.TOT.CON.DR.AMT
********************************************************************************    

    JOB.REC<BTB.JOB.TOT.EX.LC.AMT> = Y.TOT.LC.AMOUNT
    JOB.REC<BTB.JOB.TOT.NET.FOB.VALUE> = Y.TOT.FOB.VALUE
    JOB.REC<BTB.JOB.TOT.EX.LC.DRAW.AMT> = Y.TOT.DR.AMT
    Y.TEMP = JOB.REC<BTB.JOB.EX.LC.SHIP.DATE>
    Y.TEMP<1,Y.LC.SHIP.DATE.POS> = Y.LATEST.SHIP.DATE
    JOB.REC<BTB.JOB.EX.LC.SHIP.DATE> = Y.TEMP
    JOB.REC<BTB.JOB.TOT.PC.ENT.AMT> = Y.TOT.PC.VALUE
    JOB.REC<BTB.JOB.TOT.BTB.ENT.AMT> = Y.TOT.BTB.VALUE
    
    

*    WRITE JOB.REC TO F.JOB,Y.JOB.NUM
    EB.DataAccess.FWrite(FN.JOB, Y.JOB.NUM, JOB.REC)
RETURN
*--------------------------------------- --------------------------------------
SALE.CONTACT:

    Y.CON.AMOUNT     = JOB.REC<BTB.JOB.CONT.AMOUNT> ;*ASOSIATE MULTIVALUE FIELD
    Y.CON.AMT.SUM    = SUM(Y.CON.AMOUNT)
    Y.TOT.CON.AMOUNT = DROUND(Y.CON.AMT.SUM,2)
    Y.CON.FOB.VALUE     = JOB.REC<BTB.JOB.CONT.NET.FOB.VALUE> ;*ASOSIATE MULTIVALUE FIELD
    Y.TOT.CON.FOB.SUM   = SUM(Y.CON.FOB.VALUE)
    Y.TOT.CON.FOB.VALUE = DROUND(Y.TOT.CON.FOB.SUM,2)
    
    Y.PC.VALUE     = JOB.REC<BTB.JOB.CONT.PC.ENTLMNT> ;*ASOSIATE MULTIVALUE FIELD
    Y.TOT.PC.SUM   = SUM(Y.PC.VALUE)
    Y.TOT.PC.VALUE = DROUND(Y.TOT.PC.SUM,2)
    
    Y.BTB.VALUE     = JOB.REC<BTB.JOB.CONT.BTB.ENTLMNT> ;*ASOSIATE MULTIVALUE FIELD
    Y.TOT.BTB.SUM   = SUM(Y.BTB.VALUE)
    Y.TOT.BTB.VALUE = DROUND(Y.TOT.BTB.SUM,2)
    
    Y.CON.DR.REF        = JOB.REC<BTB.JOB.COLL.DR.REFNO> ;*ASOSIATE MULTIVALUE FIELD
    
    CONVERT SM TO VM IN Y.CON.DR.REF
    
    Y.CON.DR.COUNT = DCOUNT(Y.CON.DR.REF, @VM)
    Y.CON.DR.AMT = '0'
    FOR I = 1 TO Y.CON.DR.COUNT
        Y.CON.DR.ID = Y.CON.DR.REF<1,I>
        EB.DataAccess.FRead(FN.DR, Y.CON.DR.ID, CON.DR.REC, F.DR, CON.DR.ERR)
        Y.CON.DR.AMT = CON.DR.REC<LC.Contract.Drawings.TfDrDocumentAmount>
        Y.TOT.CON.DR.AMT += Y.CON.DR.AMT
    NEXT I
 
RETURN

END
