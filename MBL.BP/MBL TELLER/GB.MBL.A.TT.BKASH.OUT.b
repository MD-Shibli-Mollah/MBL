* @ValidationCode : MjotMTgzOTEzNTIyOTpDcDEyNTI6MTYyNTg0MDYzMzM0ODp1c2VyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMTcxMC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 09 Jul 2021 20:23:53
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE GB.MBL.A.TT.BKASH.OUT

*Subroutine Description: Auth Routine attached to VERSION.CONTROL of TELLER
*Subroutine Type:
*Attached To    :  VERSION.CONTROL of TELLER
*Attached As    :  Auth Routine
*-----------------------------------------------------------------------------
* Modification History 1 :
* 09/07/2021 -                            Retrofit   - MD. SHIBLI MOLLAH,
*                                                 FDS Bangladesh Limited
*------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_GTS.COMMON
    
    $USING AC.AccountOpening
    $USING TT.Contract
    $USING FT.Contract
    $USING ST.Customer
    
* $USING EB.DataAccess
    $USING EB.SystemTables
    

*    Y.CR.AC.ID = R.NEW(TT.TE.ACCOUNT.2)
    Y.CR.AC.ID = EB.SystemTables.getRNew(TT.Contract.Teller.TeAccountTwo)
*
*    Y.TXN.CODE = R.NEW(TT.TE.TRANSACTION.CODE)
    Y.TXN.CODE = EB.SystemTables.getRNew(TT.Contract.Teller.TeTransactionCode)

       
* IF V$FUNCTION NE "A" OR  Y.CR.AC.ID NE 111613120722698 THEN RETURN
    IF EB.SystemTables.getVFunction() NE "A" OR  Y.CR.AC.ID NE 111613120722698 THEN
        RETURN
    END

*  Y.REC.STATUS=R.NEW(TT.TE.RECORD.STATUS)
    Y.REC.STATUS=EB.SystemTables.getRNew(TT.Contract.Teller.TeRecordStatus)

    IF Y.REC.STATUS EQ "RNAU" THEN
        RETURN
    END

*    Y.NARR=R.NEW(TT.TE.NARRATIVE.2)
    Y.NARR=EB.SystemTables.getRNew(TT.Contract.Teller.TeNarrativeTwo)

    !FN.AC = 'F.ACCOUNT'
    !F.AC = ''
    !CALL OPF(FN.AC,F.AC)

    !FN.CUS = 'F.CUSTOMER'
    !F.CUS = ''
    !CALL OPF(FN.CUS,F.CUS)

    Y.CR.AC.ID = ''
    Y.CR.SMS.OUT = ''
    Y.CR.FILE.NAME = ''

* Y.TT.ID = ID.NEW
    Y.TT.ID = EB.SystemTables.getIdNew()
    R.TT.REC = ''
    Y.TT.ERR = ''

    Y.TODAY = EB.SystemTables.getToday()
    TIME.ST = TIMEDATE()
    Y.DATE.TIME =Y.TODAY:'_':TIME.ST[1,2]:TIME.ST[4,2]:TIME.ST[7,2]
    Y.DT.TM = Y.TODAY:' ':TIME.ST[1,2]:':':TIME.ST[4,2]

* Y.VAL = R.NEW(TT.TE.TRANSACTION.CODE)
    Y.VAL = EB.SystemTables.getRNew(TT.Contract.Teller.TeTransactionCode)

    !ETEXT = Y.NARR:'2. For bKash A/C CRD, Narr. Format:XXXX-DDD'
    !CALL STORE.END.ERROR


    BEGIN CASE

        CASE Y.VAL EQ 10
            GOSUB TT.CRAC.FILE.GEN
        CASE Y.VAL EQ 35
            GOSUB TT.CRAC.FILE.GEN

    END CASE

RETURN

TT.CRAC.FILE.GEN:
******************


* Y.CR.AC.ID = R.NEW(TT.TE.ACCOUNT.2)
    Y.CR.AC.ID = EB.SystemTables.getRNew(TT.Contract.Teller.TeAccountTwo)

* Y.CR.CUS.ID = R.NEW(TT.TE.CUSTOMER.2)
    Y.CR.CUS.ID = EB.SystemTables.getRNew(TT.Contract.Teller.TeCustomerTwo)
    !CALL F.READ(FN.CUS,Y.CR.CUS.ID,R.TT.CR.CUS.REC,F.CUS,Y.CUS.ERR)
    !Y.TT.CUS.CELL.CR = R.TT.CR.CUS.REC<EB.CUS.SMS.1>

    !CALL F.READ(FN.AC,Y.CR.AC.ID,R.CR.AC.REC,F.AC,Y.CR.AC.ERR)
    !Y.CR.AC.BR = R.CR.AC.REC<AC.CO.CODE>


    !DEBUG

    Y.SMS.BP = '/mblapp01fs/t24appprod/bnk/bnk.run/tcupload/BKASH'

    OPEN Y.SMS.BP TO F.SMS.OUT ELSE
        RETURN
    END

    !LOCATE R.CR.AC.REC<AC.CATEGORY> IN CAT.LIST SETTING Y.POS THEN

    !Y.CR.SMS.OUT = Y.TT.ID:'#':Y.DT.TM:'#':Y.VAL:'#':R.NEW(TT.TE.NARRATIVE.2):'#':R.NEW(TT.TE.ACCOUNT.2):'#':'CR':'#':R.NEW(TT.TE.AMOUNT.LOCAL.1):'#':Y.TT.CUS.CELL.CR:'#':R.CR.AC.REC<AC.ONLINE.ACTUAL.BAL>:'#':Y.CR.CUS.ID:'#':R.CR.AC.REC<AC.CATEGORY>:'#':R.NEW(TT.TE.CO.CODE)
*  Y.CR.SMS.OUT = Y.TT.ID:'#':Y.DT.TM:'#':Y.VAL:'#':R.NEW(TT.TE.NARRATIVE.2):'#':R.NEW(TT.TE.ACCOUNT.2):'#':'CR':'#':R.NEW(TT.TE.AMOUNT.LOCAL.1):'#':'':'#':'':'#':'':'#':'':'#':R.NEW(TT.TE.CO.CODE)
    Y.CR.SMS.OUT = Y.TT.ID:'#':Y.DT.TM:'#':Y.VAL:'#':EB.SystemTables.getRNew(TT.Contract.Teller.TeNarrativeTwo):'#':EB.SystemTables.getRNew(TT.Contract.Teller.TeAccountTwo):'#':'CR':'#':EB.SystemTables.getRNew(TT.Contract.Teller.TeAmountLocalOne):'#':'':'#':'':'#':'':'#':'':'#':EB.SystemTables.getRNew(TT.Contract.Teller.TeCoCode)
    Y.CR.FILE.NAME = 'SMS_':Y.TT.ID:".":'CR':'.':Y.DATE.TIME

    WRITE Y.CR.SMS.OUT TO F.SMS.OUT,Y.CR.FILE.NAME

RETURN
