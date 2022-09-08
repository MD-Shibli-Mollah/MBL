SUBROUTINE TF.MBL.A.GEN.JOB.NUMBER
*-----------------------------------------------------------------------------
*Subroutine Description:
*Subroutine Type:
*Attached To    : LETTER.OF.CREDIT Version (LETTER.OF.CREDIT,MBL.BTBRECORD)
*Attached As    : INPUT ROUTINE
*-----------------------------------------------------------------------------
* Modification History :
* 06/11/2019 -                            Retrofit   - MD. EBRAHIM KHALIL RIAN,
*                                                 FDS Bangladesh Limited
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $USING LC.Contract
    $USING ST.CompanyCreation
    $INSERT I_F.BD.BTB.JOB.REGISTER
    $INSERT I_F.BD.BTB.CUSTOMER.SEQ.NO
    $INSERT I_F.BD.LC.AD.CODE
    
    $USING EB.LocalReferences
    $USING EB.DataAccess
    $USING EB.SystemTables
    $USING EB.ErrorProcessing
*-----------------------------------------------------------------------------
    IF (EB.SystemTables.getRNew(LC.Contract.LetterOfCredit.TfLcOperation) EQ "O" OR EB.SystemTables.getRNew(LC.Contract.LetterOfCredit.TfLcOperation) EQ "A") AND V$FUNCTION EQ "A" THEN
	    GOSUB INITIALISE ; *INITIALISATION
        GOSUB OPENFILE ; *FILE OPEN
	    GOSUB PROCESS ; *PROCESS BUSINESS LOGIC
    END
*-----------------------------------------------------------------------------

*** <region name= INITIALISE>
INITIALISE:
*** <desc>INITIALISATION </desc>
    FN.BD.BTB.JOB.REGISTER = 'F.BD.BTB.JOB.REGISTER'
    F.BD.BTB.JOB.REGISTER = ''
    
    FN.BD.BTB.JOB.CUS.SEQNO = 'F.BD.BTB.CUSTOMER.SEQ.NO'
    F.BD.BTB.JOB.CUS.SEQNO = ''
   
    FN.BD.LC.AD.CODE= 'F.BD.LC.AD.CODE'
    F.BD.LC.AD.CODE = ''
    
    FN.COM = 'F.COMPANY'
    F.COM = ''
    
    Y.NEW.EXIST = ''
    Y.JOB.NO = ''
    Y.LC.TYPE = ''
    Y.CUST.SEQNO = ''
    Y.SERIAL.NO = ''
    Y.LC.CURR = ''
    Y.JOB.CURR = ''
    Y.JOB.EXC.RATE = ''
    Y.NET.FOB.AMT = ''
    Y.BTB.ENT.PER = ''
    Y.PC.ENT.PER = ''
    Y.BTB.ENT.AMT = ''
    Y.PC.ENT.AMT = ''
RETURN
*** </region>


*-----------------------------------------------------------------------------

*** <region name= OPENFILE>
OPENFILE:
*** <desc>FILE OPEN </desc>
    EB.DataAccess.Opf(FN.BD.BTB.JOB.REGISTER,F.BD.BTB.JOB.REGISTER)
    EB.DataAccess.Opf(FN.BD.BTB.JOB.CUS.SEQNO,F.BD.BTB.JOB.CUS.SEQNO)
    EB.DataAccess.Opf(FN.BD.LC.AD.CODE,F.BD.LC.AD.CODE)
    EB.DataAccess.Opf(FN.COM,F.COM)
RETURN
*** </region>

*** <region name= PROCESS>
PROCESS:
*** <desc>PROCESS BUSINESS LOGIC </desc>
    GOSUB GET.LOC.REF
    Y.NEW.EXIST = EB.SystemTables.getRNew(LC.Contract.LetterOfCredit.TfLcLocalRef)<1,Y.NEW.EXIST.POS>
    Y.LC.CURR = EB.SystemTables.getRNew(LC.Contract.LetterOfCredit.TfLcLcCurrency)
    Y.JOB.CURR = EB.SystemTables.getRNew(LC.Contract.LetterOfCredit.TfLcLocalRef)<1,Y.JOB.CUR.POS>
    Y.JOB.EXC.RATE = EB.SystemTables.getRNew(LC.Contract.LetterOfCredit.TfLcLocalRef)<1,Y.JOB.EXC.RATE.POS>
    Y.JOB.NO = EB.SystemTables.getRNew(LC.Contract.LetterOfCredit.TfLcLocalRef)<1,Y.JOB.NO.POS>
    Y.LC.TYPE = EB.SystemTables.getRNew(LC.Contract.LetterOfCredit.TfLcLcType)
    EB.DataAccess.FRead(FN.COM,EB.SystemTables.getIdCompany(),R.COM.REC,F.COM,Y.COM.ERR)

    Y.BTB.ENT.PER = EB.SystemTables.getRNew(LC.Contract.LetterOfCredit.TfLcLocalRef)<1,Y.BTB.RATE.POS>
    Y.PC.ENT.PER = EB.SystemTables.getRNew(LC.Contract.LetterOfCredit.TfLcLocalRef)<1,Y.PC.RATE.POS>

*****************************************Modification by erian@fortress-global.com*******DATE:20201010*****************
    Y.NET.FOB.AMT = EB.SystemTables.getRNew(LC.Contract.LetterOfCredit.TfLcLocalRef)<1,Y.FOB.VALUE.POS>
    Y.BTB.ENT.AMT = EB.SystemTables.getRNew(LC.Contract.LetterOfCredit.TfLcLocalRef)<1,Y.BTB.ENT.POS>
    Y.PC.ENT.AMT = EB.SystemTables.getRNew(LC.Contract.LetterOfCredit.TfLcLocalRef)<1,Y.PC.ENT.POS>
    
*    IF Y.JOB.CURR NE Y.LC.CURR THEN
*        Y.CCY.MKT = '1'
*        Y.CCY.BUY = Y.LC.CURR
*        Y.BUY.AMT = EB.SystemTables.getRNew(LC.Contract.LetterOfCredit.TfLcLocalRef)<1,Y.FOB.VALUE.POS>
*        Y.CCY.SELL = Y.JOB.CURR
*        Y.SELL.AMT = ''
*        Y.BASE.CCY = ''
*        Y.EXCHANGE.RATE = Y.JOB.EXC.RATE
*        Y.DIFFERENCE = ''
*        Y.LCY.AMT = ''
*        Y.RETURN.CODE = ''
*        CALL EXCHRATE(Y.CCY.MKT,Y.CCY.BUY,Y.BUY.AMT,Y.CCY.SELL,Y.SELL.AMT,Y.BASE.CCY,Y.EXCHANGE.RATE,Y.DIFFERENCE,Y.LCY.AMT,Y.RETURN.CODE)
******************************************Modification by erian@fortress-global.com*******DATE:20200929*****************
*        Y.NET.FOB.AMT = Y.BUY.AMT
**Y.NET.FOB.AMT = Y.SELL.AMT
**************************END****************Modification by erian@fortress-global.com*******DATE:20200929*****************
*        Y.BTB.ENT.AMT = (Y.SELL.AMT * Y.BTB.ENT.PER)/100
*        Y.PC.ENT.AMT = (Y.SELL.AMT * Y.PC.ENT.PER)/100
*    END ELSE
*        Y.NET.FOB.AMT = EB.SystemTables.getRNew(LC.Contract.LetterOfCredit.TfLcLocalRef)<1,Y.FOB.VALUE.POS>
*        Y.BTB.ENT.AMT = EB.SystemTables.getRNew(LC.Contract.LetterOfCredit.TfLcLocalRef)<1,Y.BTB.ENT.POS>
*        Y.PC.ENT.AMT = EB.SystemTables.getRNew(LC.Contract.LetterOfCredit.TfLcLocalRef)<1,Y.PC.ENT.POS>
*    END
**********************END*******************Modification by erian@fortress-global.com*******DATE:20201010*****************
    BEGIN CASE
        CASE Y.NEW.EXIST EQ "NEW"
            EB.DataAccess.FRead(FN.BD.LC.AD.CODE,EB.SystemTables.getIdCompany(),R.BD.LC.AD.CODE,F.BD.LC.AD.CODE,BD.LC.AD.CODE.ERR)
            IF BD.LC.AD.CODE.ERR THEN
                EB.SystemTables.setE("AD CODE DOES NOT EXIST FOR THIS COMPANY")
                EB.ErrorProcessing.StoreEndError()
                RETURN
            END ELSE
                Y.AD.CODE = R.BD.LC.AD.CODE<AD.CODE.AD.CODE>
            END

            Y.CUSTOMER = EB.SystemTables.getRNew(LC.Contract.LetterOfCredit.TfLcBeneficiaryCustno)
            EB.DataAccess.FRead(FN.BD.BTB.JOB.CUS.SEQNO,Y.CUSTOMER,R.BD.BTB.JOB.CUS.SEQNO,F.BD.BTB.JOB.CUS.SEQNO,Y.CUST.SEQ.ERR)
            IF R.BD.BTB.JOB.CUS.SEQNO THEN
                Y.CUST.SEQNO = R.BD.BTB.JOB.CUS.SEQNO<BD.LC.SEQ.NO> + 1
            END ELSE
                Y.CUST.SEQNO = "1"
            END
            R.BD.BTB.JOB.CUS.SEQNO<BD.LC.SEQ.NO> = Y.CUST.SEQNO
            !EB.DataAccess.FWrite(FN.BD.BTB.CUSTOMER.SEQ.NO,Y.CUSTOMER,R.BD.BTB.CUSTOMER.SEQ.NO)
            !CALL CONCAT.FILE.UPDATE(FN.BD.BTB.JOB.CUS.SEQNO,Y.CUSTOMER,Y.CUST.SEQNO,'I','AL')
            WRITE R.BD.BTB.JOB.CUS.SEQNO TO F.BD.BTB.JOB.CUS.SEQNO,Y.CUSTOMER
            Y.SERIAL.NO = ''
            Y.JOB.NO = ''
            BEGIN CASE
                CASE LEN(Y.CUST.SEQNO) EQ 1
                    Y.SERIAL.NO = "0000":Y.CUST.SEQNO
                CASE LEN(Y.CUST.SEQNO) EQ 2
                    Y.SERIAL.NO = "000":Y.CUST.SEQNO
                CASE LEN(Y.CUST.SEQNO) EQ 3
                    Y.SERIAL.NO = "00":Y.CUST.SEQNO
                CASE LEN(Y.CUST.SEQNO) EQ 4
                    Y.SERIAL.NO = "0":Y.CUST.SEQNO
                CASE LEN(Y.CUST.SEQNO) EQ 5
                    Y.SERIAL.NO = Y.CUST.SEQNO
            END CASE

            Y.JOB.NO = Y.AD.CODE:".":Y.CUSTOMER:".":Y.SERIAL.NO:".":EB.SystemTables.getToday()[3,2]
            Y.TEMP = EB.SystemTables.getRNew(LC.Contract.LetterOfCredit.TfLcLocalRef)
            Y.TEMP<1,Y.JOB.NO.POS> = Y.JOB.NO
            EB.SystemTables.setRNew(LC.Contract.LetterOfCredit.TfLcLocalRef, Y.TEMP)
            Y.COUNT = "1"
            R.BTB.JOB.REGISTER<BTB.JOB.CUSTOMER.NO> = EB.SystemTables.getRNew(LC.Contract.LetterOfCredit.TfLcBeneficiaryCustno)
            R.BTB.JOB.REGISTER<BTB.JOB.JOB.CREATE.DATE> = EB.SystemTables.getToday()
            R.BTB.JOB.REGISTER<BTB.JOB.JOB.CURRENCY> = EB.SystemTables.getRNew(LC.Contract.LetterOfCredit.TfLcLocalRef)<1,Y.JOB.CUR.POS>
            R.BTB.JOB.REGISTER<BTB.JOB.JOB.EXPIRY.DATE> = EB.SystemTables.getRNew(LC.Contract.LetterOfCredit.TfLcAdviceExpiryDate)

*********changed by Huraira.20201109*******
****no Segrigation for Sales Contract & Export LC. all should update in Export LC part of JOB Register
*            IF Y.LC.TYPE EQ "SCNT" THEN
*                GOSUB UPDATE.JOB.REGISTER.CONTRACT
*            END ELSE
*                GOSUB UPDATE.JOB.REGISTER.EXPORTLC
*            END
            GOSUB UPDATE.JOB.REGISTER.EXPORTLC
*********changed by Huraira.20201109*******

        CASE Y.NEW.EXIST = "EXIST"
            IF Y.JOB.NO EQ "" THEN
                EB.SystemTables.setE("JOB NUMBER MISSING")
                EB.ErrorProcessing.StoreEndError()
                RETURN
            END
            EB.DataAccess.FRead(FN.BD.BTB.JOB.REGISTER,Y.JOB.NO,R.BTB.JOB.REGISTER,F.BD.BTB.JOB.REGISTER,R.BTB.JOB.REG.ERR)
            IF R.BTB.JOB.REG.ERR THEN
                EB.SystemTables.setE("JOB NUMBER DOES NOT EXIST")
                EB.ErrorProcessing.StoreEndError()
                RETURN
            END
            IF EB.SystemTables.getRNew(LC.Contract.LetterOfCredit.TfLcAdviceExpiryDate) GT R.BTB.JOB.REGISTER<BTB.JOB.JOB.EXPIRY.DATE> THEN
                R.BTB.JOB.REGISTER<BTB.JOB.JOB.EXPIRY.DATE> = EB.SystemTables.getRNew(LC.Contract.LetterOfCredit.TfLcAdviceExpiryDate)
            END
        
*********changed by Huraira.20201109*******
*            IF Y.LC.TYPE EQ "SCNT" THEN
*                GOSUB GET.VM.COUNT.CONTRACT
*                GOSUB UPDATE.JOB.REGISTER.CONTRACT
*            END ELSE
*                GOSUB GET.VM.COUNT.EXPORTLC
*                GOSUB UPDATE.JOB.REGISTER.EXPORTLC
*            END
            GOSUB GET.VM.COUNT.EXPORTLC
            GOSUB UPDATE.JOB.REGISTER.EXPORTLC
*********changed by Huraira.20201109*******
    END CASE
RETURN
*** </region>

*** <region name= GET.VM.COUNT.EXPORTLC>
GET.VM.COUNT.EXPORTLC:
*** <desc>GET.VM.COUNT.EXPORTLC </desc>
    Y.EX.TF.NO = R.BTB.JOB.REGISTER<BTB.JOB.EX.TF.REF>

*********changed by Huraira.20201109*******
*****always for new LC record count increase by 1. no need to locat for existing record since New LC recording every time
*    LOCATE EB.SystemTables.getIdNew() IN Y.EX.TF.NO<1,1> SETTING Y.COUNT.POS THEN
*        Y.COUNT = Y.COUNT.POS
*        R.BTB.JOB.REGISTER<BTB.JOB.TOT.BTB.ENT.AMT> -= R.BTB.JOB.REGISTER<BTB.JOB.EX.BTB.ENT.AMT,Y.COUNT>
*        R.BTB.JOB.REGISTER<BTB.JOB.TOT.BTB.AVL.AMT> -= R.BTB.JOB.REGISTER<BTB.JOB.EX.BTB.ENT.AMT,Y.COUNT>
*
*    END ELSE
*        Y.JOB.FOUND = "0"
*        Y.COUNT = DCOUNT(Y.EX.TF.NO,@VM) + 1
*    END
    Y.COUNT = DCOUNT(Y.EX.TF.NO,@VM) + 1
*********changed by Huraira.20201109*******

RETURN
*** </region>

*** <region name= GET.VM.COUNT.CONTRACT>
GET.VM.COUNT.CONTRACT:
*** <desc>GET.VM.COUNT.CONTRACT </desc>
*    Y.CONT.TF.NO = R.BTB.JOB.REGISTER<BTB.JOB.CON.TF.REF.NO>
*    LOCATE EB.SystemTables.getIdNew() IN Y.CONT.TF.NO<1,1> SETTING Y.COUNT.POS THEN
*        Y.COUNT = Y.COUNT.POS
*        R.BTB.JOB.REGISTER<BTB.JOB.TOT.BTB.ENT.AMT> -= R.BTB.JOB.REGISTER<BTB.JOB.EX.BTB.ENT.AMT,Y.COUNT>
*        R.BTB.JOB.REGISTER<BTB.JOB.TOT.BTB.AVL.AMT> -= R.BTB.JOB.REGISTER<BTB.JOB.EX.BTB.ENT.AMT,Y.COUNT>
*    END ELSE
*        Y.JOB.FOUND = "0"
*        Y.COUNT = DCOUNT(Y.CONT.TF.NO,@VM) + 1
**    END
RETURN
*** </region>

*** <region name= GET.LOC.REF>
GET.LOC.REF:
*** <desc>GET.LOC.REF </desc>
    Y.JOB.NO.POS=''
    Y.JOB.ENT.POS=''
    Y.NEW.EXIST.POS=''
    Y.LC.AMT.LCY.POS = ''
    Y.EXCH.RATE.POS = ''
    EB.LocalReferences.GetLocRef("LETTER.OF.CREDIT","LT.TF.JOB.NUMBR",Y.JOB.NO.POS)
    EB.LocalReferences.GetLocRef("LETTER.OF.CREDIT","LT.TF.BTB.CNTNO",Y.CONT.NO.POS)
    EB.LocalReferences.GetLocRef("LETTER.OF.CREDIT","LT.TF.BTB.ENTRT",Y.BTB.RATE.POS)
    EB.LocalReferences.GetLocRef("LETTER.OF.CREDIT","LT.TF.PC.ENT.RT",Y.PC.RATE.POS)
    EB.LocalReferences.GetLocRef("LETTER.OF.CREDIT","LT.TF.BTB.ENTAM",Y.BTB.ENT.POS)
    EB.LocalReferences.GetLocRef("LETTER.OF.CREDIT","LT.TF.PC.ENT.AM",Y.PC.ENT.POS)
    EB.LocalReferences.GetLocRef("LETTER.OF.CREDIT","LT.TF.NW.EX.JOB",Y.NEW.EXIST.POS)
    EB.LocalReferences.GetLocRef("LETTER.OF.CREDIT","LT.TF.NET.FOBVL",Y.FOB.VALUE.POS)
    EB.LocalReferences.GetLocRef("LETTER.OF.CREDIT","LT.TF.JOB.ENCUR",Y.JOB.CUR.POS)
    EB.LocalReferences.GetLocRef("LETTER.OF.CREDIT","LT.TF.JOB.EX.RT",Y.JOB.EXC.RATE.POS)
RETURN
*** </region>

*** <region name= UPDATE.JOB.REGISTER.EXPORTLC>
UPDATE.JOB.REGISTER.EXPORTLC:
*** <desc>UPDATE.JOB.REGISTER.EXPORTLC </desc>
    R.BTB.JOB.REGISTER<BTB.JOB.EX.TF.REF,Y.COUNT> = EB.SystemTables.getIdNew()
    R.BTB.JOB.REGISTER<BTB.JOB.EX.ISSUE.DATE,Y.COUNT> = EB.SystemTables.getRNew(LC.Contract.LetterOfCredit.TfLcIssueDate)
    R.BTB.JOB.REGISTER<BTB.JOB.EX.LC.NUMBER,Y.COUNT> = EB.SystemTables.getRNew(LC.Contract.LetterOfCredit.TfLcIssBankRef)
    R.BTB.JOB.REGISTER<BTB.JOB.EX.LC.CURRENCY,Y.COUNT> = EB.SystemTables.getRNew(LC.Contract.LetterOfCredit.TfLcLcCurrency)
    
*********changed by Jobayer.20201130*******
    Y.LC.AMT=EB.SystemTables.getRNew(LC.Contract.LetterOfCredit.TfLcLcAmount)
    R.BTB.JOB.REGISTER<BTB.JOB.EX.LC.AMOUNT,Y.COUNT> = Y.LC.AMT
*******
    R.BTB.JOB.REGISTER<BTB.JOB.EX.NET.FOB.VALUE,Y.COUNT> = Y.NET.FOB.AMT
    R.BTB.JOB.REGISTER<BTB.JOB.EX.BTB.ENT.PER,Y.COUNT> = Y.BTB.ENT.PER
    R.BTB.JOB.REGISTER<BTB.JOB.EX.BTB.ENT.AMT,Y.COUNT> = Y.BTB.ENT.AMT
    R.BTB.JOB.REGISTER<BTB.JOB.EX.PC.ENT.PER,Y.COUNT> = Y.PC.ENT.PER
    R.BTB.JOB.REGISTER<BTB.JOB.EX.PC.ENT.AMT,Y.COUNT> = Y.PC.ENT.AMT
    R.BTB.JOB.REGISTER<BTB.JOB.EX.EXPIRY.DATE,Y.COUNT> = EB.SystemTables.getRNew(LC.Contract.LetterOfCredit.TfLcAdviceExpiryDate)

*   R.BTB.JOB.REGISTER<BTB.JOB.EX.BTB.AVL.AMT,Y.COUNT> += Y.BTB.ENT.AMT
*   R.BTB.JOB.REGISTER<BTB.JOB.EX.PC.AVL.AMT,Y.COUNT> += Y.PC.ENT.AMT
    R.BTB.JOB.REGISTER<BTB.JOB.EX.BTB.AVL.AMT,Y.COUNT> = Y.BTB.ENT.AMT
    R.BTB.JOB.REGISTER<BTB.JOB.EX.PC.AVL.AMT,Y.COUNT> = Y.PC.ENT.AMT

    R.BTB.JOB.REGISTER<BTB.JOB.TOT.PC.ENT.AMT> += Y.PC.ENT.AMT
    R.BTB.JOB.REGISTER<BTB.JOB.TOT.PC.AVL.AMT> += Y.PC.ENT.AMT
    R.BTB.JOB.REGISTER<BTB.JOB.TOT.BTB.ENT.AMT> += Y.BTB.ENT.AMT
    R.BTB.JOB.REGISTER<BTB.JOB.TOT.BTB.AVL.AMT> += Y.BTB.ENT.AMT
     
    
*********changed by Jobayer.20201130*******
    R.BTB.JOB.REGISTER<BTB.JOB.TOT.EX.LC.AMT> += Y.LC.AMT
    R.BTB.JOB.REGISTER< BTB.JOB.TOT.NET.FOB.VALUE> += Y.NET.FOB.AMT
*******

    EB.DataAccess.FWrite(FN.BD.BTB.JOB.REGISTER,Y.JOB.NO,R.BTB.JOB.REGISTER)
RETURN
*** </region>


*** <region name= UPDATE.JOB.REGISTER.CONTRACT>
UPDATE.JOB.REGISTER.CONTRACT:
*** <desc>UPDATE.JOB.REGISTER.CONTRACT</desc>
*    R.BTB.JOB.REGISTER<BTB.JOB.CON.TF.REF.NO,Y.COUNT> = EB.SystemTables.getIdNew()
*    R.BTB.JOB.REGISTER<BTB.JOB.CON.CONTACT.NO,Y.COUNT> = EB.SystemTables.getRNew(LC.Contract.LetterOfCredit.TfLcLocalRef)<1,Y.CONT.NO.POS>
*    R.BTB.JOB.REGISTER<BTB.JOB.CON.CONT.CURR,Y.COUNT> = EB.SystemTables.getRNew(LC.Contract.LetterOfCredit.TfLcLcCurrency)
*    R.BTB.JOB.REGISTER<BTB.JOB.CON.CONT.AMT,Y.COUNT> = EB.SystemTables.getRNew(LC.Contract.LetterOfCredit.TfLcLcAmount)
*    R.BTB.JOB.REGISTER<BTB.JOB.CON.ISSUE.DATE,Y.COUNT> = EB.SystemTables.getRNew(LC.Contract.LetterOfCredit.TfLcIssueDate)
*    R.BTB.JOB.REGISTER<BTB.JOB.CON.LC.OR.CONT,Y.COUNT> = "CONTRACT"
*    R.BTB.JOB.REGISTER<BTB.JOB.CON.REP.ALLOW,Y.COUNT> = "YES"
*    R.BTB.JOB.REGISTER<BTB.JOB.CON.OUTST.AMT,Y.COUNT> = EB.SystemTables.getRNew(LC.Contract.LetterOfCredit.TfLcLcAmount)
*    R.BTB.JOB.REGISTER<BTB.JOB.CON.NET.FOB.VALUE,Y.COUNT> = Y.NET.FOB.AMT
*    R.BTB.JOB.REGISTER<BTB.JOB.CON.BTB.ENT.PER,Y.COUNT> = Y.BTB.ENT.PER
*    R.BTB.JOB.REGISTER<BTB.JOB.CON.BTB.ENT.AMT,Y.COUNT> = Y.BTB.ENT.AMT
*    R.BTB.JOB.REGISTER<BTB.JOB.CON.PC.ENT.PER,Y.COUNT> = Y.PC.ENT.PER
*    R.BTB.JOB.REGISTER<BTB.JOB.CON.PC.ENT.AMT,Y.COUNT> = Y.PC.ENT.AMT
*    R.BTB.JOB.REGISTER<BTB.JOB.CON.EXP.DATE,Y.COUNT> = EB.SystemTables.getRNew(LC.Contract.LetterOfCredit.TfLcExpiryDate)
*    R.BTB.JOB.REGISTER<BTB.JOB.CON.BTB.AVL.AMT,Y.COUNT> += Y.BTB.ENT.AMT
*    R.BTB.JOB.REGISTER<BTB.JOB.CON.PC.AVL.AMT,Y.COUNT> += Y.PC.ENT.AMT

*********changed by Huraira.20201109*******
****no update for entitlement for specific Sales Contract type LC
*    R.BTB.JOB.REGISTER<BTB.JOB.TOT.PC.ENT.AMT> += Y.PC.ENT.AMT
*    R.BTB.JOB.REGISTER<BTB.JOB.TOT.PC.AVL.AMT> += Y.PC.ENT.AMT
*    R.BTB.JOB.REGISTER<BTB.JOB.TOT.BTB.ENT.AMT> += Y.BTB.ENT.AMT
*    R.BTB.JOB.REGISTER<BTB.JOB.TOT.BTB.AVL.AMT> += Y.BTB.ENT.AMT
*    EB.DataAccess.FWrite(FN.BD.BTB.JOB.REGISTER,Y.JOB.NO,R.BTB.JOB.REGISTER)
*********changed by Huraira.20201109*******

RETURN
*** </region>
END
