SUBROUTINE TF.MBL.E.NOF.CUS.WISE.LC.INFO(Y.RETURN)
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------

*-----------------------------------------------------------------------------
    $INSERT  I_COMMON
    $INSERT  I_EQUATE
    $INSERT  I_ENQUIRY.COMMON
*$INSERT  I_F.LETTER.OF.CREDIT
    $USING  LC.Contract
* $INSERT  I_F.DRAWINGS
* $INSERT  I_F.CURRENCY 
    $USING ST.CurrencyConfig
    $USING EB.Utility
    $USING EB.Updates
    $USING EB.Reports
    $USING EB.SystemTables
    $USING EB.DataAccess
    $USING EB.API
    $USING EB.Display

    GOSUB INIT
    GOSUB PROCESS
    GOSUB CLEAR.DATA
RETURN

INIT:
    FN.LC='F.LETTER.OF.CREDIT'
    FP.LC=''

    FN.DR='F.DRAWINGS'
    FP.DR=''

    FN.CUR='F.CURRENCY'
    FP.CUR=''
    Y.COM.ID=EB.SystemTables.getIdCompany()

    EB.DataAccess.Opf(FN.LC,FP.LC)

    EB.DataAccess.Opf(FN.DR,FP.DR)
    EB.DataAccess.Opf(FN.CUR,FP.CUR)
    Y.CATEGORY.LIST='23025':@FM:'23010':@FM:'2390':@FM:'23055':@FM:'23070':@FM:'23045':@FM:'23085':@FM:'23092':@FM:'23091':@FM:'23065':@FM:'23245':@FM:'23260':@FM:'23275':@FM:'23305':@FM:'23280':@FM:'23260':@FM:'23250':@FM:'23255':@FM:'23240':@FM:'23295':@FM:'23300':@FM:'23290':@FM:'23285':@FM:'23295'

    *Y.CUS.ID = EB.Reports.getEnqSelection<4,1>
    !Y.CUR.ID='USD'
     LOCATE 'Y.CUS.ID' IN EB.Reports.getEnqSelection()<2,1> SETTING CUS.POS THEN
        Y.CUS.ID = EB.Reports.getEnqSelection()<4,CUS.POS>
    END
RETURN
 
PROCESS:
    
    IF Y.CUS.ID NE '' THEN
        SEL.CMD="SELECT " :FN.LC :" WITH APPLICANT.CUSTNO EQ ":Y.CUS.ID:" AND CO.CODE EQ ":Y.COM.ID:" BY CATEGORY.CODE"
    END ELSE
        SEL.CMD="SELECT " :FN.LC:" WITH CO.CODE EQ ":Y.COM.ID:" BY CATEGORY.CODE"
    END

    
    EB.DataAccess.Readlist(SEL.CMD,SEL.LIST,'',NO.OF.REC,ERR.NO)
    LOOP
        REMOVE Y.LC.ID FROM SEL.LIST SETTING Y.POS
    WHILE Y.LC.ID:Y.POS

        EB.DataAccess.FRead(FN.LC,Y.LC.ID,R.LC.REC,FP.LC,LC.ERR)

        Y.CATEGORY.CODE = R.LC.REC<LC.Contract.LetterOfCredit.TfLcCategoryCode>
        FIND Y.CATEGORY.CODE IN Y.CATEGORY.LIST SETTING Y.POS1,Y.POS2 THEN

            Y.CUS = R.LC.REC<LC.Contract.LetterOfCredit.TfLcApplicantCustno>

            Y.APPLICANT = R.LC.REC<LC.Contract.LetterOfCredit.TfLcApplicant>

            Y.LC.NO = R.LC.REC<LC.Contract.LetterOfCredit.TfLcOldLcNumber>

            Y.LC.AMT =R.LC.REC<LC.Contract.LetterOfCredit.TfLcLcAmount>

            Y.OUT.BAL = R.LC.REC<LC.Contract.LetterOfCredit.TfLcLiabilityAmt>

            Y.LC.CCY = R.LC.REC<LC.Contract.LetterOfCredit.TfLcLcCurrency>

            IF Y.LC.CCY EQ 'BDT' THEN
                Y.BCC.RATE=1
                Y.INT.RATE=1

                Y.LC.BDT =DROUND(R.LC.REC<LC.Contract.LetterOfCredit.TfLcLiabilityAmt> * 1)
            END ELSE

                EB.DataAccess.FRead(FN.CUR,Y.LC.CCY,R.CUR.REC,FP.CUR,CUR.ERR)

                Y.CUR.MARKET=R.CUR.REC<ST.CurrencyConfig.Currency.EbCurCurrencyMarket>
                Y.CUR.CNT=DCOUNT(Y.CUR.MARKET,@VM)

                Y.INT.RATE=R.CUR.REC<ST.CurrencyConfig.Currency.EbCurMidRevalRate,Y.CUR.CNT>

                Y.LC.BDT =DROUND(R.LC.REC<LC.Contract.LetterOfCredit.TfLcLiabilityAmt> * Y.INT.RATE)
                Y.BCC.RATE=Y.INT.RATE
            END


            Y.EXPIRY.DATE =R.LC.REC<LC.Contract.LetterOfCredit.TfLcExpiryDate>

            Y.AUTHORISER=R.LC.REC<LC.Contract.LetterOfCredit.TfLcAuthoriser>

            SEL.DR.CMD = "SELECT " :FN.DR :" WITH LC.NUMBER MATCHES ":Y.LC.ID:'...'

            EB.DataAccess.Readlist(SEL.DR.CMD,SEL.DR.LIST,'',DR.OF.REC,DR.ERR)

            IF SEL.DR.LIST NE '' THEN
                LOOP
                    REMOVE Y.DR.ID FROM SEL.DR.LIST SETTING Y.DR.POS
                WHILE Y.DR.ID:Y.DR.POS

                    EB.DataAccess.FRead(FN.DR,Y.DR.ID,R.DR.REC,FP.DR,DR.ERR)
                    Y.DRA.ID=Y.DR.ID

                    Y.DOCUMENT.AMOUNT=R.DR.REC<LC.Contract.Drawings.TfDrDocumentAmount>

                    Y.DR.BDT =DROUND(R.DR.REC<LC.Contract.Drawings.TfDrDocumentAmount> *Y.INT.RATE)

                    Y.MATURITY.REVIEW=R.DR.REC<LC.Contract.Drawings.TfDrMaturityReview>
                    Y.BALANCE=Y.LC.AMT-Y.DOCUMENT.AMOUNT

                    Y.DRAWING.TYPE=R.DR.REC<LC.Contract.Drawings.TfDrDrawingType>

                    IF Y.DRAWING.TYPE NE 'SP' AND Y.DRAWING.TYPE NE 'MA' AND Y.DRAWING.TYPE NE 'FR' THEN
                        GOSUB RESULT
                        GOSUB CLEAR.DATA
                    END ELSE
                        Y.DRA.ID=''
                        Y.DR.BDT =''
                        Y.MATURITY.REVIEW=''
                        Y.BALANCE=''
                        Y.DOCUMENT.AMOUNT=''
                        Y.DRAWING.TYPE=''
                       ! GOSUB RESULT
                    END
                REPEAT
                *GOSUB RESULT
            END ELSE
                Y.DRA.ID=''
                Y.DR.BDT =''
                Y.MATURITY.REVIEW=''
                Y.BALANCE=''
                Y.DOCUMENT.AMOUNT=''
                Y.DRAWING.TYPE=''
                GOSUB RESULT
                GOSUB CLEAR.DATA
            END
            
        END
        ELSE
            Y.POS1 = ''
            Y.POS2 = ''
        END

    REPEAT
RETURN

RESULT:

    IF Y.LC.ID NE '' OR Y.DRA.ID NE '' THEN
        Y.RETURN<-1> = Y.CUS:'*':Y.LC.ID:'*':Y.APPLICANT:'*':Y.LC.NO:'*':Y.EXPIRY.DATE:'*':Y.CATEGORY.CODE:'*':Y.LC.AMT:'*':Y.OUT.BAL:'*':Y.BCC.RATE:'*':Y.LC.CCY:'*':Y.LC.BDT:'*':Y.DRA.ID:'*':Y.DOCUMENT.AMOUNT:'*':Y.DR.BDT:'*':Y.MATURITY.REVIEW:'*':Y.BALANCE:'*':Y.DRAWING.TYPE
        !PRINT Y.RETURN
    END
RETURN

CLEAR.DATA:
    Y.LC.ID =''
    Y.APPLICANT =''
    Y.LC.NO =''
    Y.EXPIRY.DATE =''
    !Y.CATEGORY.CODE =''
    Y.LC.AMT =''
    Y.OUT.BAL=''
    Y.BCC.RATE =''
    Y.LC.CCY =''
    Y.LC.BDT =''
    Y.DRA.ID =''
    Y.DOCUMENT.AMOUNT =''
    Y.DR.BDT =''
    Y.MATURITY.REVIEW =''
    Y.BALANCE =''
    Y.DRAWING.TYPE=''
    Y.AUTHORISER=''
    !   Y.INT.RATE=''
    Y.BCC.RATE=''
RETURN

END
