SUBROUTINE TF.MBL.I.LD.EXP.DOC.PUR
*-----------------------------------------------------------------------------
*Subroutine Description: This routine is fatch record from DRAWINGS
*Subroutine Type:
*Attached To    : ACTIVITY API
*Attached As    : INPUT ROUTINE
*-----------------------------------------------------------------------------
* Modification History :
* 27/04/2020 -                            Retrofit   - MD. EBRAHIM KHALIL RIAN,
*                                                 FDS Bangladesh Limited
*
* 20/04/2012 - Rayhan
* 20/08/2013 - Ayush - Commented from line no 80 to 91 as it is not required.
* 02/10/2013 - Ayush - Added code to get the exchange rate from currency
*                      market 15 and populate. Tenor also needs to be populated
*                      from drawings to LD.
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INCLUDE I_F.BD.BTB.JOB.REGISTER
*
*    $INCLUDE T24.BP I_F.LETTER.OF.CREDIT
*    $INCLUDE T24.BP I_F.LD.LOANS.AND.DEPOSITS
*    $INCLUDE T24.BP I_F.LC.TYPES
*    $INCLUDE T24.BP I_F.DRAWINGS
*    $INCLUDE T24.BP I_F.CURRENCY
    
    $USING LC.Contract
    $USING LC.Config
    $USING ST.CurrencyConfig
    
    
    $USING EB.DataAccess
    $USING EB.SystemTables
    $USING EB.ErrorProcessing
    $USING EB.OverrideProcessing
    $USING EB.LocalReferences
    $USING AA.Account
    $USING AA.Framework
    $USING EB.API
*-----------------------------------------------------------------------------
    GOSUB INITIALISE ; *INITIALISATION
    GOSUB OPENFILE ; *FILE OPEN
    GOSUB PROCESS ; *PROCESS BUSINESS LOGIC
RETURN
*-----------------------------------------------------------------------------

*** <region name= INITIALISE>
INITIALISE:
*** <desc>INITIALISATION </desc>
    FN.LETTER.OF.CREDIT = 'F.LETTER.OF.CREDIT'
    F.LETTER.OF.CREDIT  = ''
    R.LETTER.OF.CREDIT  = ''

    FN.LD.LOANS.AND.DEPOSITS = 'F.LD.LOANS.AND.DEPOSITS'
    F.LD.LOANS.AND.DEPOSITS  = ''
    R.LD.LOANS.AND.DEPOSITS  = ''


    FN.BD.BTB.JOB.REGISTER = 'F.BD.BTB.JOB.REGISTER'
    F.BD.BTB.JOB.REGISTER = ''
    R.BD.BTB.JOB.REGISTER = ''

    FN.DRAWINGS = 'F.DRAWINGS'
    F.DRAWINGS = ''
    R.DRAWINGS = ''
    Y.DRAWINGS.ERR = ''

    FN.LC.TYPES = 'F.LC.TYPES'
    F.LC.TYPES = ''
    R.LC.TYPES = ''
    Y.LC.TYPES.ERR = ''

    FN.CURR = 'F.CURRENCY'
    F.CURR = ''

    Y.CCY = ''
    R.CCY.REC = ''
    Y.CCY.ERR = ''
    Y.EXCHANGE.RATE = ''
    Y.CCY.MKT = 15
    Y.CCY.MARKET = ''
    Y.MID.REVAL.RATE = ''
    Y.CCY.POS = ''
    Y.DR.EXCHANGE.RATE = 1
    Y.ARR.EXCHANGE.RATE = 1
    
    EB.LocalReferences.GetLocRef("LETTER.OF.CREDIT","LT.TF.BTB.CNTNO",Y.CONT.NO.POS)
    EB.LocalReferences.GetLocRef("LETTER.OF.CREDIT","LT.TF.JOB.NUMBR",Y.JOB.NO.POS)
    EB.LocalReferences.GetLocRef("DRAWINGS","LT.TF.DOC.TYPE",Y.DR.DOC.TY.POS)
    EB.LocalReferences.GetLocRef("DRAWINGS","LT.TF.TENOR",Y.DR.TENOR.POS)
    EB.LocalReferences.GetLocRef("DRAWINGS","LT.TF.ELC.COLNO",Y.EXLC.COLL.NO.POS)
    
    EB.LocalReferences.GetLocRef('AA.ARR.ACCOUNT','LT.AC.LINK.TFNO',Y.LD.TFNO.POS)
    EB.LocalReferences.GetLocRef('AA.ARR.ACCOUNT','LT.LN.BIL.DOCVL',Y.LD.DOC.VAL.POS)
    EB.LocalReferences.GetLocRef("AA.ARR.ACCOUNT","LT.LN.PUR.FCAMT",Y.LD.PUR.AMT.POS)
    EB.LocalReferences.GetLocRef("AA.ARR.ACCOUNT","LT.TF.EXCH.RATE",Y.LD.XRATE.POS)
    EB.LocalReferences.GetLocRef("AA.ARR.ACCOUNT","LT.TF.EXP.LC.NO",Y.LD.EXP.LCNO.POS)
    EB.LocalReferences.GetLocRef("AA.ARR.ACCOUNT","LT.TF.BTB.CNTNO",Y.LD.CONT.NO.POS)
    EB.LocalReferences.GetLocRef("AA.ARR.ACCOUNT","LT.TF.JOB.NUMBR",Y.LD.JOB.NO.POS)

    
    EB.LocalReferences.GetLocRef("AA.ARR.ACCOUNT","LT.TF.TENOR",Y.LD.TENOR.POS)
    
    EB.LocalReferences.GetLocRef("AA.ARR.ACCOUNT",'LT.LEGACY.ID',Y.OLD.LEGACY.ID.POS)
    EB.LocalReferences.GetLocRef("AA.ARR.ACCOUNT",'LINKED.TFDR.REF',Y.LINKED.TFDR.REF.POS)
RETURN
*** </region>


*-----------------------------------------------------------------------------

*** <region name= OPENFILE>
OPENFILE:
*** <desc>FILE OPEN </desc>
    EB.DataAccess.Opf(FN.LETTER.OF.CREDIT,F.LETTER.OF.CREDIT)
    EB.DataAccess.Opf(FN.BD.BTB.JOB.REGISTER,F.BD.BTB.JOB.REGISTER)
    EB.DataAccess.Opf(FN.DRAWINGS,F.DRAWINGS)
    EB.DataAccess.Opf(FN.LC.TYPES,F.LC.TYPES)
    EB.DataAccess.Opf(FN.CURR,F.CURR)
RETURN
*** </region>

*** <region name= PROCESS>
PROCESS:
*** <desc>PROCESS BUSINESS LOGIC </desc>
    
    APP.NAME = 'AA.ARR.ACCOUNT'
    EB.API.GetStandardSelectionDets(APP.NAME, R.SS)
    Y.FIELD.NAME = 'LOCAL.REF'
    LOCATE Y.FIELD.NAME IN R.SS<AA.Account.Account.AcLocalRef> SETTING Y.POS THEN
    END
    CALL AA.GET.ACCOUNT.RECORD(R.PROPERTY.RECORD, PROPERTY.ID)
    TMP.DATA = R.PROPERTY.RECORD<1,Y.POS>
    Y.LINKED.TFDR.REF = FIELD(TMP.DATA,SM, Y.LINKED.TFDR.REF.POS)
    

    EB.DataAccess.FRead(FN.DRAWINGS,Y.LINKED.TFDR.REF,R.DRAWINGS.REC,F.DRAWINGS,DRAW.ERR)
    Y.EXLC.COLL.NO = R.DRAWINGS.REC<LC.Contract.Drawings.TfDrLocalRef, Y.EXLC.COLL.NO.POS>
    

    Y.DR.ID = Y.LINKED.TFDR.REF
    Y.LC.REF.NO = Y.DR.ID[1,12]
    EB.DataAccess.FRead(FN.LETTER.OF.CREDIT,Y.LC.REF.NO,R.LETTER.OF.CREDIT,F.LETTER.OF.CREDIT,Y.LETTER.OF.CREDIT.ERR)
    IF R.LETTER.OF.CREDIT EQ '' THEN RETURN

    Y.EXP.LC.NO = R.LETTER.OF.CREDIT<LC.Contract.LetterOfCredit.TfLcIssBankRef>
    Y.CON.NO = R.LETTER.OF.CREDIT<LC.Contract.LetterOfCredit.TfLcLocalRef, Y.CONT.NO.POS>
    Y.CUSTO = R.LETTER.OF.CREDIT<LC.Contract.LetterOfCredit.TfLcBeneficiaryCustno>
    Y.JOB.NO = R.LETTER.OF.CREDIT<LC.Contract.LetterOfCredit.TfLcLocalRef, Y.JOB.NO.POS>
    Y.LC.TYPE = R.LETTER.OF.CREDIT<LC.Contract.LetterOfCredit.TfLcLcType>
    Y.LC.CATEGORY = R.LETTER.OF.CREDIT<LC.Contract.LetterOfCredit.TfLcCategoryCode>
    

    EB.DataAccess.FRead(FN.LC.TYPES,Y.LC.TYPE,R.LC.TYPES,F.LC.TYPES,Y.LC.TYPES.ERR)
    Y.LC.IMP.EXP = R.LC.TYPES<LC.Config.Types.TypImportExport>

    IF Y.LC.IMP.EXP EQ 'E' THEN
        EB.DataAccess.FRead(FN.DRAWINGS,Y.DR.ID,R.DRAWINGS,F.DRAWINGS,ERR.DRAWINGS)
        Y.CCY = R.DRAWINGS<LC.Contract.Drawings.TfDrDrawCurrency>
        Y.AMT = R.DRAWINGS<LC.Contract.Drawings.TfDrDocumentAmount>
        Y.BILL.DOC.VAL = Y.CCY : Y.AMT

*---------------------------------------------
        IF Y.CCY NE 'BDT' THEN
            Y.DR.TENOR = EB.SystemTables.getRNew(LC.Contract.Drawings.TfDrLocalRef)<1,Y.DR.TENOR.POS>
            EB.DataAccess.FRead(FN.CURR,Y.CCY,R.CCY.REC,F.CURR,Y.CCY.ERR)
            Y.CCY.MARKET = R.CCY.REC<ST.CurrencyConfig.Currency.EbCurCurrencyMarket>
            Y.MID.REVAL.RATE = R.CCY.REC<ST.CurrencyConfig.Currency.EbCurMidRevalRate>
            IF R.CCY.REC THEN
                LOCATE Y.CCY.MKT IN Y.CCY.MARKET<1,1> SETTING Y.CCY.POS THEN
                    Y.DR.EXCHANGE.RATE = Y.MID.REVAL.RATE<1,Y.CCY.POS>
                END
            END
        END
      
        Y.ARR.CUR = AA.Framework.getC_aalocarrcurrency() ;*-----------------
    
        IF Y.ARR.CUR NE 'BDT' THEN
            EB.DataAccess.FRead(FN.CURR,Y.ARR.CUR,R.NEW.CCY.REC,F.CURR,Y.NEW.CCY.ERR)
            Y.ARR.CCY.MARKET = R.NEW.CCY.REC<ST.CurrencyConfig.Currency.EbCurCurrencyMarket>
            Y.ARR.MID.REVAL.RATE = R.NEW.CCY.REC<ST.CurrencyConfig.Currency.EbCurMidRevalRate>
            IF R.NEW.CCY.REC THEN
                LOCATE Y.CCY.MKT IN Y.ARR.CCY.MARKET<1,1> SETTING Y.NEW.CCY.POS THEN
                    Y.ARR.EXCHANGE.RATE = Y.ARR.MID.REVAL.RATE<1,Y.NEW.CCY.POS>
                END
            END
        END

        Y.EXCHANGE.RATE = Y.DR.EXCHANGE.RATE/Y.ARR.EXCHANGE.RATE
               
*-------------------------------------------
    
        Y.TEMP = EB.SystemTables.getRNew(AA.Account.Account.AcLocalRef)
        Y.TEMP<1,Y.OLD.LEGACY.ID.POS>=Y.EXLC.COLL.NO
        Y.TEMP<1,Y.LD.EXP.LCNO.POS> = Y.EXP.LC.NO
        Y.TEMP<1,Y.LD.TFNO.POS> = Y.LC.REF.NO
        Y.TEMP<1,Y.LD.CONT.NO.POS> = Y.CON.NO
        Y.TEMP<1,Y.LD.JOB.NO.POS> = Y.JOB.NO
        Y.TEMP<1,Y.LD.DOC.VAL.POS> = Y.BILL.DOC.VAL

        Y.TEMP<1,Y.LD.XRATE.POS> = Y.EXCHANGE.RATE
        Y.TEMP<1,Y.LD.TENOR.POS> = Y.DR.TENOR
        EB.SystemTables.setRNew(AA.Account.Account.AcLocalRef, Y.TEMP)
    END ELSE
        EB.SystemTables.setEtext("Not Export LC Type")
        EB.ErrorProcessing.StoreEndError()
    END
    
RETURN
*** </region>

END
