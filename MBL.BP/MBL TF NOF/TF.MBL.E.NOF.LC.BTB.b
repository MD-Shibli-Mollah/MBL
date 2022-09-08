* @ValidationCode : Mjo1MzA0ODkwNTM6Q3AxMjUyOjE1OTY3MDY3MTIwNDg6dXNlcjotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDE3MTAuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 06 Aug 2020 15:38:32
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0
*-----------------------------------------------------------------------------
* <Rating>-91</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE TF.MBL.E.NOF.LC.BTB(Y.RETURN)
*PROGRAM E.LC.BTB.TEST
    $INSERT  I_COMMON
    $INSERT  I_EQUATE
    $INSERT  I_ENQUIRY.COMMON
    $USING  ST.CompanyCreation
    $USING  LC.Contract
* $INSERT T24.BP I_F.LC.TYPES
    $USING LC.Config
    !$INSERT T24.BP I_F.CATEGORY
    $USING ST.Config
    $USING EB.Utility
    $USING EB.SystemTables
    $USING EB.DataAccess
    $USING EB.API
    $USING EB.Display
    $USING EB.Updates
    $USING EB.Foundation
    $USING EB.Reports
*
*
    GOSUB INIT
    GOSUB OPENFILES
    GOSUB PROCESS
RETURN
*---------
INIT:
*---------
*
    FN.LC ='F.LETTER.OF.CREDIT'
    F.LC =''
*
    FN.LC.TP.DES = 'F.LC.TYPES'
    F.LC.TP.DES =''
*
    FN.CATE = 'F.CATEGORY'
    F.CATE =''
*
    FN.LC.HIS ='F.LETTER.OF.CREDIT$HIS'
    F.LC.HIS =''
*

    Y.TYPE ='BACL BDCF BFDC BFEU BFMC BFMU BIEC BMCF BMCL BSCF BSCL BSNE BSNF BSNI BSNL BSNT BSNZ BSPE BSPF BSPI BSPL BSPT BSPZ BSUF BUAE BUAF BUAI BUAL BUAT BUAZ BUNE BUNF BUNI BUNL BUNT BUNZ BZDC BZSC BZSE'
*    Y.TYPE ='BSCF BSCL BACL BDCF BZSU BZSC BIEC BZSE BFMU BFDC BZDC BFEU BFMC BMCF BMCL'
    !Y.TYPE = 'BFDC'
    Y.PRE.LC.TYPE =""
    Y.CUR.LC.TYPE =""
    Y.PRE.CATE =""
    Y.CUR.CATE =""
    Y.FROM.DATE =''
    Y.TO.DATE =''
    Y.CUS.NO =''
RETURN
*----------
OPENFILES:
*----------
*
    EB.DataAccess.Opf(FN.LC,F.LC)
    EB.DataAccess.Opf(FN.LC.TP.DES,F.LC.TP.DES)
    EB.DataAccess.Opf(FN.CATE,F.CATE)
    EB.DataAccess.Opf(FN.LC.HIS,F.LC.HIS)
RETURN
*
*---------
PROCESS:
*---------
    LOCATE 'FROM.DATE' IN EB.Reports.getEnqSelection()<2,1> SETTING FROM.POS THEN
        Y.FROM.DATE = EB.Reports.getEnqSelection()<4,FROM.POS>
    END
    LOCATE 'TO.DATE' IN EB.Reports.getEnqSelection()<2,1> SETTING TO.POS THEN
        Y.TO.DATE = EB.Reports.getEnqSelection()<4,TO.POS>
    END
    LOCATE 'CUS.NO' IN EB.Reports.getEnqSelection()<2,1> SETTING CUS.POS THEN
        Y.CUS.NO = EB.Reports.getEnqSelection()<4,CUS.POS>
    END
    Y.COMPANY = EB.SystemTables.getIdCompany()
    IF  Y.TO.DATE = "" THEN
        Y.TO.DATE = Y.FROM.DATE
    END
*
    IF Y.CUS.NO EQ '' AND Y.FROM.DATE NE '' AND Y.TO.DATE NE '' THEN
        SEL.CMD ='SELECT ':FN.LC :' WITH LC.TYPE EQ ':Y.TYPE:' AND CO.CODE EQ ':Y.COMPANY:' AND ISSUE.DATE GE ':Y.FROM.DATE:' AND ISSUE.DATE LE ':Y.TO.DATE:' BY CATEGORY.CODE BY LC.CURRENCY BY ISSUE.DATE'
        EB.DataAccess.Readlist(SEL.CMD,SEL.LIST,'',NO.OF.REC,RET.CODE)
    END
    ELSE IF Y.CUS.NO NE '' AND Y.FROM.DATE EQ '' AND Y.TO.DATE EQ '' THEN
        SEL.CMD ='SELECT ':FN.LC :' WITH LC.TYPE EQ ':Y.TYPE:' AND CO.CODE EQ ':Y.COMPANY:' AND CON.CUS.LINK EQ ':Y.CUS.NO:' BY CATEGORY.CODE BY LC.CURRENCY BY ISSUE.DATE'
        EB.DataAccess.Readlist(SEL.CMD,SEL.LIST,'',NO.OF.REC,RET.CODE)
    END
    ELSE
        SEL.CMD ='SELECT ':FN.LC :' WITH LC.TYPE EQ ':Y.TYPE:' AND CO.CODE EQ ':Y.COMPANY:' BY CATEGORY.CODE BY LC.CURRENCY BY ISSUE.DATE'
        EB.DataAccess.Readlist(SEL.CMD,SEL.LIST,'',NO.OF.REC,RET.CODE)
    END
*
    Y.LIST=SEL.LIST
    !Y.CNT=0
    IF NO.OF.REC EQ 0 THEN
        GOSUB GET.HIS.MAIN.DATA
    END ELSE
        LOOP
            REMOVE Y.LC.ID FROM SEL.LIST SETTING POS
        WHILE Y.LC.ID : POS
***outerloop start
            !Y.CNT++
            EB.DataAccess.FRead(FN.LC,Y.LC.ID,R.LC,F.LC,Y.ERR)
*
            Y.LC.NO = Y.LC.ID
            !Y.LC.TYPE = R.LC<TF.LC.LC.TYPE>
            Y.LC.TYPE = R.LC<LC.Contract.LetterOfCredit.TfLcLcType>
            ! Y.LC.CURR = R.LC<TF.LC.LC.CURRENCY>
            Y.LC.CURR = R.LC<LC.Contract.LetterOfCredit.TfLcLcCurrency>
            !Y.LC.AMT = R.LC<TF.LC.LC.AMOUNT>
            Y.LC.AMT = R.LC<LC.Contract.LetterOfCredit.TfLcLcAmount>
            !Y.ISSUE.DATE = R.LC<TF.LC.ISSUE.DATE>
            Y.ISSUE.DATE = R.LC<LC.Contract.LetterOfCredit.TfLcIssueDate>
            !  Y.CATE = R.LC<TF.LC.CATEGORY.CODE>
            Y.CATE = R.LC<LC.Contract.LetterOfCredit.TfLcCategoryCode>
            !Y.CUS.NAME = R.LC<TF.LC.APPLICANT,1>
            Y.CUS.NAME = R.LC<LC.Contract.LetterOfCredit.TfLcApplicant,1>
            Y.COUNT='1'
            Y.COUNT.OUT.BAL='1'
*
            IF NO.OF.REC EQ '1' THEN
                Y.ISS.DT = Y.ISSUE.DATE
*
                GOSUB GET.LC.CATE.DES
                GOSUB GET.HIS.DATA
*
                Y.RETURN<-1> = Y.ISSUE.DATE:"*":Y.CATE.DES:"*":Y.LC.TYPE:"*":Y.COUNT:"*":Y.LC.CURR:"*":Y.LC.AMT:"*":Y.FROM.DATE:"*":Y.TO.DATE:"*":Y.CUS.NO:"*":Y.CUS.NAME:"*":Y.CATE
                !CRT Y.ISSUE.DATE:"*":Y.CATE.DES:"*":Y.LC.TYPE:"*":Y.COUNT:"*":Y.LC.CURR:"*":Y.LC.AMT:"*":Y.FROM.DATE:"*":Y.TO.DATE:"*":Y.CUS.NO:"*":Y.CUS.NAME
                Y.TOT.DRAW.AMT=''
                Y.DRAW.CURR=''
                Y.TOT.COUNT.DR.AMT=''
            END ELSE
*
                GOSUB GET.LC.TYPE
*
            END
*
            IF NO.OF.REC NE '1' THEN

                Y.RETURN<-1> =  Y.ISSUE.DATE:"*":Y.CATE.DES:"*":Y.PRE.LC.TYPE:"*":Y.TOT.COUNT:"*":Y.PRE.LC.CURR:"*":Y.TOT.LC.AMT:"*":Y.FROM.DATE:"*":Y.TO.DATE:"*":Y.CUS.NO:"*":Y.CUS.NAME:"*":Y.PRE.CATE
                !CRT Y.ISSUE.DATE:"*":Y.CATE.DES:"*":Y.PRE.LC.TYPE:"*":Y.TOT.COUNT:"*":Y.PRE.LC.CURR:"*":Y.TOT.LC.AMT:"*":Y.FROM.DATE:"*":Y.TO.DATE:"*":Y.CUS.NO:"*":Y.CUS.NAME
                Y.TOT.DRAW.AMT=''
                Y.DRAW.CURR=''
                Y.TOT.COUNT.DR.AMT=''
            END
***outer loop end
        REPEAT
        RETURN

*----------------
GET.LC.CATE.DES:
*----------------
        EB.DataAccess.FRead(FN.CATE,Y.CATE,R.CATE,F.CATE,Y.CATE.ERR)
        !Y.CATE.DES = R.CATE<EB.CAT.DESCRIPTION>
        Y.CATE.DES = R.CATE<ST.Config.Category.EbCatDescription>
        RETURN

*-----------
GET.LC.TYPE:
*-----------

        IF Y.PRE.CATE EQ '' AND Y.CUR.CATE EQ '' THEN

            Y.PRE.LC.TYPE=Y.LC.TYPE
            Y.PRE.LC.CURR=Y.LC.CURR
            Y.CUR.LC.TYPE=Y.LC.TYPE
            Y.CUR.LC.CURR=Y.LC.CURR

            Y.PRE.CATE=Y.CATE
            Y.CUR.CATE=Y.CATE

            Y.ISS.DT = Y.ISSUE.DATE
            Y.TOT.COUNT=Y.TOT.COUNT+Y.COUNT

            Y.TOT.LC.AMT = Y.TOT.LC.AMT+Y.LC.AMT

            GOSUB GET.LC.CATE.DES
            GOSUB GET.HIS.DATA

        END ELSE
            Y.CUR.LC.TYPE=Y.LC.TYPE
            Y.CUR.LC.CURR=Y.LC.CURR
            Y.CUR.CATE=Y.CATE

            IF (Y.PRE.CATE NE Y.CUR.CATE) OR (Y.PRE.LC.CURR NE Y.CUR.LC.CURR) THEN
                !IF (Y.PRE.CATE NE Y.CUR.CATE) OR (Y.PRE.LC.CURR NE Y.CUR.LC.CURR) OR (Y.ISS.DT NE Y.ISSUE.DATE) THEN

*Y.RETURN<-1> =  Y.ISS.DT:"*":Y.CATE.DES:"*":Y.PRE.LC.TYPE:"*":Y.TOT.COUNT:"*":Y.PRE.LC.CURR:"*":Y.TOT.LC.AMT:"*":Y.FROM.DATE:"*":Y.TO.DATE:"*":Y.CUS.NO:"*":Y.CUS.NAME:"*":Y.PRE.CATE
                !CRT Y.ISS.DT:"*":Y.CATE.DES:"*":Y.PRE.LC.TYPE:"*":Y.TOT.COUNT:"*":Y.PRE.LC.CURR:"*":Y.TOT.LC.AMT:"*":Y.FROM.DATE:"*":Y.TO.DATE:"*":Y.CUS.NO:"*":Y.CUS.NAME

                Y.TOT.DRAW.AMT=''
                Y.DRAW.CURR=''
                Y.ISS.DT = Y.ISSUE.DATE
                Y.TOT.COUNT=''
                Y.TOT.COUNT.DR.AMT=''
                Y.TOT.COUNT=Y.TOT.COUNT+Y.COUNT

                Y.PRE.CATE=Y.CATE

                Y.PRE.LC.TYPE = Y.LC.TYPE
                Y.PRE.LC.CURR = Y.LC.CURR
                Y.TOT.LC.AMT = ''
                Y.TOT.LC.LIB.AMT = ''
                Y.TOT.LC.AMT = Y.TOT.LC.AMT+Y.LC.AMT

                GOSUB GET.LC.CATE.DES
                GOSUB GET.HIS.DATA

            END ELSE

                Y.ISS.DT = Y.ISSUE.DATE
                Y.TOT.COUNT=Y.TOT.COUNT+Y.COUNT

                Y.PRE.CATE=Y.CATE

                Y.PRE.LC.TYPE = Y.LC.TYPE
                Y.PRE.LC.CURR = Y.LC.CURR
                Y.TOT.LC.AMT = Y.TOT.LC.AMT+Y.LC.AMT

                GOSUB GET.LC.CATE.DES

            END
        END
        RETURN
    END

*------------
GET.HIS.DATA:
*------------

    IF Y.CUS.NO EQ '' AND Y.FROM.DATE NE '' AND Y.TO.DATE NE '' THEN
        SEL.HIS.DATA ='SELECT ':FN.LC.HIS :' WITH RECORD.STATUS EQ ':"MAT":' AND LC.CURRENCY EQ ':Y.LC.CURR:' AND LC.TYPE EQ ':Y.LC.TYPE:' AND CO.CODE EQ ':Y.COMPANY:' AND ISSUE.DATE GE ':Y.FROM.DATE:' AND ISSUE.DATE LE ':Y.TO.DATE:' BY CATEGORY.CODE BY LC.CURRENCY BY ISSUE.DATE BY-DSND @ID'
        EB.DataAccess.Readlist(SEL.HIS.DATA,SEL.LIST.HIS,'',NO.OF.HIS.REC,HIS.ERR)
    END
    ELSE IF Y.CUS.NO NE '' AND Y.FROM.DATE EQ '' AND Y.TO.DATE EQ '' THEN
        SEL.HIS.DATA ='SELECT ':FN.LC.HIS :' WITH RECORD.STATUS EQ ':"MAT":' AND LC.CURRENCY EQ ':Y.LC.CURR:' AND LC.TYPE EQ ':Y.LC.TYPE:' AND CO.CODE EQ ':Y.COMPANY:' AND CON.CUS.LINK EQ ':Y.CUS.NO:' BY CATEGORY.CODE BY LC.CURRENCY BY ISSUE.DATE BY-DSND @ID'
        EB.DataAccess.Readlist(SEL.HIS.DATA,SEL.LIST.HIS,'',NO.OF.HIS.REC,HIS.ERR)
    END
    ELSE
        SEL.HIS.DATA ='SELECT ':FN.LC.HIS :' WITH RECORD.STATUS EQ ':"MAT":' AND LC.CURRENCY EQ ':Y.LC.CURR:' AND LC.TYPE EQ ':Y.LC.TYPE:' AND CO.CODE EQ ':Y.COMPANY:' BY CATEGORY.CODE BY LC.CURRENCY BY ISSUE.DATE BY-DSND @ID'
        EB.DataAccess.Readlist(SEL.HIS.DATA,SEL.LIST.HIS,'',NO.OF.HIS.REC,HIS.ERR)
    END
    LOOP
        REMOVE Y.LC.HIS.ID FROM SEL.LIST.HIS SETTING POS2
    WHILE Y.LC.HIS.ID : POS2
        EB.DataAccess.FRead(FN.LC.HIS,Y.LC.HIS.ID,R.LC.HIS,F.LC.HIS,Y.DRAW.HIS.ERR)

        FIND Y.LC.HIS.ID[1,12] IN Y.LIST SETTING POS1,POS2 THEN
        END
        ELSE
            IF Y.DRAW.HIS.ID NE Y.LC.HIS.ID[1,12] THEN

                Y.HIS.COUNT='1'
                !Y.DRAW.HIS.AMT = R.LC.HIS<TF.LC.LC.AMOUNT>
                Y.DRAW.HIS.AMT = R.LC.HIS<LC.Contract.LetterOfCredit.TfLcLcAmount>
                Y.DRAW.HIS.ID=Y.LC.HIS.ID[1,12]
                Y.TOT.LC.AMT=Y.TOT.LC.AMT+Y.DRAW.HIS.AMT
                Y.TOT.COUNT=Y.TOT.COUNT+Y.HIS.COUNT
            END
        END
    REPEAT
RETURN

*-----------------
GET.HIS.MAIN.DATA:
*-----------------
    IF Y.CUS.NO EQ '' AND Y.FROM.DATE NE '' AND Y.TO.DATE NE '' THEN
        SEL.HIS.DATA ='SELECT ':FN.LC.HIS :' WITH RECORD.STATUS EQ ':"MAT":' AND LC.TYPE EQ ':Y.TYPE:' AND CO.CODE EQ ':Y.COMPANY:' AND ISSUE.DATE GE ':Y.FROM.DATE:' AND ISSUE.DATE LE ':Y.TO.DATE:' BY CATEGORY.CODE BY LC.CURRENCY BY ISSUE.DATE BY-DSND @ID'
        EB.DataAccess.Readlist(SEL.HIS.DATA,SEL.LIST.HIS,'',NO.OF.HIS.REC,HIS.ERR)
    END
    ELSE IF Y.CUS.NO NE '' AND Y.FROM.DATE EQ '' AND Y.TO.DATE EQ '' THEN
        SEL.HIS.DATA ='SELECT ':FN.LC.HIS :' WITH RECORD.STATUS EQ ':"MAT":' AND LC.TYPE EQ ':Y.TYPE:' AND CO.CODE EQ ':Y.COMPANY:' AND CON.CUS.LINK EQ ':Y.CUS.NO:' BY CATEGORY.CODE BY LC.CURRENCY BY ISSUE.DATE BY-DSND @ID'
        EB.DataAccess.Readlist(SEL.HIS.DATA,SEL.LIST.HIS,'',NO.OF.HIS.REC,HIS.ERR)
    END
    ELSE
        SEL.HIS.DATA ='SELECT ':FN.LC.HIS :' WITH RECORD.STATUS EQ ':"MAT":' AND LC.TYPE EQ ':Y.TYPE:' AND CO.CODE EQ ':Y.COMPANY:' BY CATEGORY.CODE BY LC.CURRENCY BY ISSUE.DATE BY-DSND @ID'
        EB.DataAccess.Readlist(SEL.HIS.DATA,SEL.LIST.HIS,'',NO.OF.HIS.REC,HIS.ERR)
    END
    LOOP
        REMOVE Y.LC.HIS.ID FROM SEL.LIST.HIS SETTING POS2
    WHILE Y.LC.HIS.ID : POS2
        EB.DataAccess.FRead(FN.LC.HIS,Y.LC.HIS.ID,R.DRAW.HIS,F.LC.HIS,Y.DRAW.HIS.ERR)

        Y.LC.NO = Y.LC.ID
        !Y.LC.TYPE = R.LC.HIS<TF.LC.LC.TYPE>
        Y.LC.TYPE = R.LC.HIS<LC.Contract.LetterOfCredit.TfLcLcType>
        !Y.LC.CURR = R.LC.HIS<TF.LC.LC.CURRENCY>
        Y.LC.CURR = R.LC.HIS<LC.Contract.LetterOfCredit.TfLcLcCurrency>
        !Y.LC.AMT = R.LC.HIS<TF.LC.LC.AMOUNT>
        Y.LC.AMT = R.LC.HIS<LC.Contract.LetterOfCredit.TfLcLcAmount>
        !Y.ISSUE.DATE = R.LC.HIS<TF.LC.ISSUE.DATE>
        Y.ISSUE.DATE = R.LC.HIS<LC.Contract.LetterOfCredit.TfLcIssueDate>
        !Y.CATE = R.LC.HIS<TF.LC.CATEGORY.CODE>
        Y.CATE = R.LC.HIS<LC.Contract.LetterOfCredit.TfLcCategoryCode>
        !Y.CUS.NAME = R.LC.HIS<TF.LC.APPLICANT,1>
        Y.CUS.NAME = R.LC.HIS<LC.Contract.LetterOfCredit.TfLcApplicant,1>
        Y.COUNT='1'
        Y.COUNT.OUT.BAL='1'



        IF NO.OF.HIS.REC EQ '1' THEN
            !GOSUB GET.LC.TP.DET

            Y.ISS.DT = Y.ISSUE.DATE

            GOSUB GET.LC.CATE.DES
            GOSUB GET.HIS.DATA

            Y.RETURN<-1> = Y.ISSUE.DATE:"*":Y.CATE.DES:"*":Y.LC.TYPE:"*":Y.COUNT:"*":Y.LC.CURR:"*":Y.LC.AMT:"*":Y.FROM.DATE:"*":Y.TO.DATE:"*":Y.CUS.NO:"*":Y.CUS.NAME:"*":Y.CATE
            !CRT Y.ISSUE.DATE:"*":Y.CATE.DES:"*":Y.LC.TYPE:"*":Y.COUNT:"*":Y.LC.CURR:"*":Y.LC.AMT:"*":Y.FROM.DATE:"*":Y.TO.DATE:"*":Y.CUS.NO:"*":Y.CUS.NAME
            Y.TOT.DRAW.AMT=''
            Y.DRAW.CURR=''
            Y.TOT.COUNT.DR.AMT=''
        END ELSE

            GOSUB GET.LC.HIS.TYPE

        END


        IF NO.OF.HIS.REC NE '1' THEN

            Y.RETURN<-1> =  Y.ISSUE.DATE:"*":Y.CATE.DES:"*":Y.PRE.LC.TYPE:"*":Y.TOT.COUNT:"*":Y.PRE.LC.CURR:"*":Y.TOT.LC.AMT:"*":Y.FROM.DATE:"*":Y.TO.DATE:"*":Y.CUS.NO:"*":Y.CUS.NAME:"*":Y.PRE.CATE
            !CRT Y.ISSUE.DATE:"*":Y.CATE.DES:"*":Y.PRE.LC.TYPE:"*":Y.TOT.COUNT:"*":Y.PRE.LC.CURR:"*":Y.TOT.LC.AMT:"*":Y.FROM.DATE:"*":Y.TO.DATE:"*":Y.CUS.NO:"*":Y.CUS.NAME
            Y.TOT.DRAW.AMT=''
            Y.DRAW.CURR=''
            Y.TOT.COUNT.DR.AMT=''
        END
***outer loop end
    REPEAT
RETURN
*---------------
GET.LC.HIS.TYPE:
*---------------

    IF Y.PRE.CATE EQ '' AND Y.CUR.CATE EQ '' THEN

        Y.PRE.LC.TYPE=Y.LC.TYPE
        Y.PRE.LC.CURR=Y.LC.CURR
        Y.CUR.LC.TYPE=Y.LC.TYPE
        Y.CUR.LC.CURR=Y.LC.CURR

        Y.PRE.CATE=Y.CATE
        Y.CUR.CATE=Y.CATE

        Y.ISS.DT = Y.ISSUE.DATE
        Y.TOT.COUNT=Y.TOT.COUNT+Y.COUNT

        Y.TOT.LC.AMT = Y.TOT.LC.AMT+Y.LC.AMT

        GOSUB GET.LC.CATE.DES
        !GOSUB GET.HIS.DATA

    END ELSE
        Y.CUR.LC.TYPE=Y.LC.TYPE
        Y.CUR.LC.CURR=Y.LC.CURR
        Y.CUR.CATE=Y.CATE

        IF (Y.PRE.CATE NE Y.CUR.CATE) OR (Y.PRE.LC.CURR NE Y.CUR.LC.CURR) THEN
            !IF (Y.PRE.CATE NE Y.CUR.CATE) OR (Y.PRE.LC.CURR NE Y.CUR.LC.CURR) OR (Y.ISS.DT NE Y.ISSUE.DATE) THEN

*Y.RETURN<-1> =  Y.ISS.DT:"*":Y.CATE.DES:"*":Y.PRE.LC.TYPE:"*":Y.TOT.COUNT:"*":Y.PRE.LC.CURR:"*":Y.TOT.LC.AMT:"*":Y.FROM.DATE:"*":Y.TO.DATE:"*":Y.CUS.NO:"*":Y.CUS.NAME:"*":Y.PRE.CATE
            !CRT Y.ISS.DT:"*":Y.CATE.DES:"*":Y.PRE.LC.TYPE:"*":Y.TOT.COUNT:"*":Y.PRE.LC.CURR:"*":Y.TOT.LC.AMT:"*":Y.FROM.DATE:"*":Y.TO.DATE:"*":Y.CUS.NO:"*":Y.CUS.NAME

            Y.TOT.DRAW.AMT=''
            Y.DRAW.CURR=''
            Y.ISS.DT = Y.ISSUE.DATE
            Y.TOT.COUNT=''
            Y.TOT.COUNT.DR.AMT=''
            Y.TOT.COUNT=Y.TOT.COUNT+Y.COUNT

            Y.PRE.CATE=Y.CATE

            Y.PRE.LC.TYPE = Y.LC.TYPE
            Y.PRE.LC.CURR = Y.LC.CURR
            Y.TOT.LC.AMT = ''
            Y.TOT.LC.LIB.AMT = ''
            Y.TOT.LC.AMT = Y.TOT.LC.AMT+Y.LC.AMT

            GOSUB GET.LC.CATE.DES
            !GOSUB GET.HIS.DATA

        END ELSE

            Y.ISS.DT = Y.ISSUE.DATE
            Y.TOT.COUNT=Y.TOT.COUNT+Y.COUNT

            Y.PRE.CATE=Y.CATE

            Y.PRE.LC.TYPE = Y.LC.TYPE
            Y.PRE.LC.CURR = Y.LC.CURR
            Y.TOT.LC.AMT = Y.TOT.LC.AMT+Y.LC.AMT

            GOSUB GET.LC.CATE.DES

        END
    END
RETURN
END