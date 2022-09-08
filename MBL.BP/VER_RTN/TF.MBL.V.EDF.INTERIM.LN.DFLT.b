* @ValidationCode : Mjo5NzEzMTUyMjY6Q3AxMjUyOjE2MDg0Njk0MDEyODY6dXNlcjotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDE3MTAuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 20 Dec 2020 19:03:21
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0
SUBROUTINE TF.MBL.V.EDF.INTERIM.LN.DFLT
*-----------------------------------------------------------------------------
* Modification History :
* 10/05/2020 -                            Retrofit   - MAHMUDUR RAHMAN UDOY,
*                                                 FDS Bangladesh Limited
*-----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    
    $USING EB.DataAccess
    $USING LC.Contract
    $USING EB.SystemTables
    $USING AA.Framework
    $USING AA.Account
    $USING EB.API
    $USING EB.Updates
    $USING EB.Utility

    

   
*-----------------------------------------------------------------------------
    GOSUB INITIALISE ; *INITIALISATION
    GOSUB OPENFILE ; *FILE OPEN
    GOSUB PROCESS ; *PROCESS BUSINESS LOGIC
RETURN
INITIALISE:
    FN.ARR = 'F.AA.ARRANGEMENT'
    F.ARR = ''
     
    FN.LC='F.LETTER.OF.CREDIT'
    F.LC = ''
    
    Y.APP="AA.ARR.ACCOUNT"
    Y.FLDS="LT.AC.DR.PUR.RN":VM:"LT.AC.BD.EXLCNO":VM:"LT.TF.TERM":VM:"LT.AC.BD.LNMADT"
    Y.POS= ''
 
    EB.Updates.MultiGetLocRef(Y.APP,Y.FLDS,Y.POS)
    Y.TF.REF.POS =Y.POS<1,1>
    Y.BB.LC.POS = Y.POS<1,2>
    Y.TERM.POS  = Y.POS<1,3>
    Y.LNMADT.POS = Y.POS<1,4>
    
    
RETURN
   
OPENFILE:
    
    EB.DataAccess.Opf(FN.ARR, F.ARR)
    EB.DataAccess.Opf(FN.LC,F.LC)
   
RETURN
   
PROCESS:

    Y.AC.LOC.REF = EB.SystemTables.getRNew(AA.Account.Account.AcLocalRef)
    Y.TF.REF = Y.AC.LOC.REF<1, Y.TF.REF.POS>
    Y.EFF.DATE = AA.Framework.getC_aalocactivityeffdate()
    
    Y.TERM = Y.AC.LOC.REF<1, Y.TERM.POS>
    Y.TERM.LEN = LEN(Y.TERM)

    Y.NUM.LEN = Y.TERM.LEN - 1
    
    Y.CK.DATE =  Y.TERM[Y.TERM.LEN,1]
    Y.CK.NUM  =  Y.TERM[1,Y.NUM.LEN]
    
    IF NUM(Y.CK.NUM) EQ 1 AND Y.CK.DATE EQ 'D' THEN
        Y.DATE = Y.TERM
        EB.Utility.CalendarDay(Y.EFF.DATE, '+', Y.DATE)
    END
*    ELSE
*        Y.DATE = '90D'
*        EB.Utility.CalendarDay(Y.EFF.DATE, '+', Y.DATE)
*    END

    


    EB.DataAccess.FRead(FN.LC, Y.TF.REF, LC.REC, F.LC, ERR.REC)
    Y.OLD.LC = LC.REC<LC.Contract.LetterOfCredit.TfLcOldLcNumber>
	Y.TEMP = EB.SystemTables.getRNew(AA.Account.Account.AcLocalRef)
    IF Y.OLD.LC NE '' THEN
        Y.TEMP<1,Y.BB.LC.POS> = Y.OLD.LC
    END
    IF Y.DATE NE '' THEN
        Y.TEMP<1,Y.LNMADT.POS> = Y.DATE
    END
    EB.SystemTables.setRNew(AA.Account.Account.AcLocalRef, Y.TEMP)
    
    EB.SystemTables.setRNew(TT.Contract.Teller.TeTellerIdOne, Y.TELLER.ID.1)
       
RETURN
 

END

