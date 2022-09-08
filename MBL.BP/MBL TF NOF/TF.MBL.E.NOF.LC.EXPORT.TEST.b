PROGRAM TF.MBL.E.NOF.LC.EXPORT.TEST

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
*$INSERT I_F.COMPANY
    $USING ST.CompanyCreation
* $INSERT I_F.LETTER.OF.CREDIT
    $USING  LC.Contract
* $INSERT I_F.LC.TYPES
    $USING LC.Config
*$INSERT I_F.CATEGORY
    $USING EB.Utility
* $INSERT I_F.DRAWINGS
    $USING EB.SystemTables
    $USING EB.DataAccess
    $USING EB.API
    $USING EB.Display
    $USING EB.Updates
    $USING EB.Foundation
    $USING EB.Reports
    

    GOSUB INIT
    GOSUB OPENFILES
    GOSUB PROCESS
    GOSUB FILE.WRITE
*GOSUB FILE.WRITE
RETURN
*---------
INIT:
*---------

    FN.LC ='FBNK.LETTER.OF.CREDIT'
    F.LC =''

    FN.LC.TP.DES = 'FBNK.LC.TYPES'
    F.LC.TP.DES =''

    !FN.CATE = 'F.LC.TYPES'
    !F.CATE =''

    FN.LC.HIS ='FBNK.LETTER.OF.CREDIT$HIS'
    F.LC.HIS =''

    FN.DR ='FBNK.DRAWINGS'
    F.DR =''

    FN.DR.HIS='FBNK.DRAWINGS$HIS'
    F.DR.HIS =''

    Y.TYPE ='CONT EAUL EDCF EFDU EFSC EMC EMU ESCF ESCL EACL EDUF EFDT EFST EMTC EMTU ESTC ESTF'
    !Y.TYPE = 'EDUF'
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
    EB.DataAccess.Opf(FN.LC,F.LC)
    EB.DataAccess.Opf(FN.LC.TP.DES,F.LC.TP.DES)
    !CALL OPF(FN.CATE,F.CATE)
    EB.DataAccess.Opf(FN.LC.HIS,F.LC.HIS)
    EB.DataAccess.Opf(FN.DR,F.DR)
    EB.DataAccess.Opf(FN.DR.HIS,F.DR.HIS)
RETURN
 
*---------
PROCESS:
*---------
*    LOCATE 'FROM.DATE' IN ENQ.SELECTION<2,1> SETTING FROM.POS THEN
*       Y.FROM.DATE =  ENQ.SELECTION<4,FROM.POS>
 
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
    !DEBUG
    IF Y.CUS.NO EQ '' AND Y.FROM.DATE NE '' AND Y.TO.DATE NE '' THEN
        SEL.CMD ='SELECT ':FN.DR :' WITH LC.CREDIT.TYPE EQ ':Y.TYPE:' AND (DRAWING.TYPE EQ ':"MA":' OR  DRAWING.TYPE EQ ':"SP":') AND CO.CODE EQ ':Y.COMPANY:' AND LT.TF.DT.PAYMNT GE ':Y.FROM.DATE:' AND LT.TF.DT.PAYMNT LE ':Y.TO.DATE:' BY LC.CREDIT.TYPE BY DRAW.CURRENCY BY LT.TF.DT.PAYMNT'
        EB.DataAccess.Readlist(SEL.CMD,SEL.LIST,'',NO.OF.REC,RET.CODE)
    END ELSE
        IF Y.CUS.NO NE '' AND Y.FROM.DATE EQ '' AND Y.TO.DATE EQ '' THEN
            SEL.CMD ='SELECT ':FN.DR :' WITH LC.CREDIT.TYPE EQ ':Y.TYPE:' AND (DRAWING.TYPE EQ ':"MA":' OR  DRAWING.TYPE EQ ':"SP":') AND CO.CODE EQ ':Y.COMPANY:' AND CUSTOMER.LINK EQ ':Y.CUS.NO:' BY LC.CREDIT.TYPE BY DRAW.CURRENCY BY LT.TF.DT.PAYMNT'
            EB.DataAccess.Readlist(SEL.CMD,SEL.LIST,'',NO.OF.REC,RET.CODE)
        END ELSE
            SEL.CMD ='SELECT ':FN.DR :' WITH LC.CREDIT.TYPE EQ ':Y.TYPE:' AND (DRAWING.TYPE EQ ':"MA":' OR  DRAWING.TYPE EQ ':"SP":') AND CO.CODE EQ ':Y.COMPANY:' AND LT.TF.DT.PAYMNT GE ':Y.FROM.DATE:' AND LT.TF.DT.PAYMNT LE ':Y.TO.DATE:' AND CUSTOMER.LINK EQ ':Y.CUS.NO: ' BY LC.CREDIT.TYPE BY DRAW.CURRENCY BY LT.TF.DT.PAYMNT'
            EB.DataAccess.Readlist(SEL.CMD,SEL.LIST,'',NO.OF.REC,RET.CODE)
        END
    END

    Y.LIST=SEL.LIST
    !Y.CNT=0
    !DEBUG
    IF NO.OF.REC EQ 0 THEN
        GOSUB GET.HIS.MAIN.DATA
    END ELSE
        LOOP
            REMOVE Y.LC.ID FROM SEL.LIST SETTING POS
        WHILE Y.LC.ID : POS
***outerloop start
            !Y.CNT++
            EB.DataAccess.FRead(FN.DR,Y.LC.ID,R.LC,F.DR,Y.ERR)

            Y.LC.NO = Y.LC.ID
*Y.LC.TYPE = R.LC<TF.DR.LC.CREDIT.TYPE>
            Y.LC.TYPE =R.LC<LC.Contract.Drawings.TfDrLcCreditType>
*Y.LC.CURR = R.LC<TF.DR.DRAW.CURRENCY>
            Y.LC.CURR =R.LC<LC.Contract.Drawings.TfDrDrawCurrency>
* Y.LC.DOC.AMT= R.LC<TF.DR.LOCAL.REF,136>
            Y.LC.DOC.AMT = R.LC<LC.Contract.Drawings.TfDrDocumentAmount>
*Y.CM.POR= R.LC<TF.DR.APP.DRAW.AMT>
            Y.CM.POR= R.LC<LC.Contract.Drawings.TfDrAppDrawAmt>
*Y.RELZ.AMT= R.LC<TF.DR.DOC.AMT.LC.CCY>
            Y.RELZ.AMT= R.LC<LC.Contract.Drawings.TfDrDocAmtLcCcy>
*Y.ISSUE.DATE = R.LC<TF.DR.VALUE.DATE>
            Y.ISSUE.DATE = R.LC<LC.Contract.Drawings.TfDrValueDate>
*Y.CATE = R.LC<TF.DR.LC.CREDIT.TYPE>
            Y.CATE =R.LC<LC.Contract.Drawings.TfDrLcCreditType>
            

            !CALL GET.LOC.REF('DRAWINGS','IMPORTER.NAME',44)
*Y.CUS.NAME=R.LC<TF.DR.LOCAL.REF,44,1>
            APPLICATION.NAMES = 'DRAWINGS'
            LOCAL.FIELDS = 'LT.TF.IMPR.NAME'
            
            EB.Foundation.MapLocalFields(APPLICATION.NAMES, LOCAL.FIELDS, FLD.POS)
            Y.CUS.NAME.POS=FLD.POS<1,1>
            Y.CUS.NAME = R.LC<LC.Contract.Drawings.TfDrLocalRef,Y.CUS.NAME.POS>
            
            Y.COUNT='1'
            Y.COUNT.OUT.BAL='1'

            IF NO.OF.REC EQ '1' THEN

                Y.ISS.DT = Y.ISSUE.DATE

                GOSUB GET.LC.CATE.DES
                GOSUB GET.HIS.DATA

                Y.RETURN<-1> = Y.ISSUE.DATE:"*":Y.CATE.DES:"*":Y.LC.TYPE:"*":Y.COUNT:"*":Y.LC.CURR:"*":Y.LC.DOC.AMT:"*":Y.FROM.DATE:"*":Y.TO.DATE:"*":Y.CUS.NO:"*":Y.CUS.NAME:"*":Y.CATE:"*":Y.RELZ.AMT:"*":Y.CM.POR
                !CRT Y.ISSUE.DATE:"*":Y.CATE.DES:"*":Y.LC.TYPE:"*":Y.COUNT:"*":Y.LC.CURR:"*":Y.LC.DOC.AMT:"*":Y.FROM.DATE:"*":Y.TO.DATE:"*":Y.CUS.NO:"*":Y.CUS.NAME:"*":Y.TOT.CM.POR:"*":Y.TOT.RELZ.AMT

                Y.TOT.DRAW.AMT=''
                Y.DRAW.CURR=''
                Y.TOT.COUNT.DR.AMT=''
            END ELSE

                GOSUB GET.LC.TYPE

            END


        IF NO.OF.REC NE '1' THEN

            Y.RETURN<-1> =  Y.ISSUE.DATE:"*":Y.CATE.DES:"*":Y.PRE.LC.TYPE:"*":Y.TOT.COUNT:"*":Y.PRE.LC.CURR:"*":Y.TOT.LC.AMT:"*":Y.FROM.DATE:"*":Y.TO.DATE:"*":Y.CUS.NO:"*":Y.CUS.NAME:"*":Y.PRE.CATE:"*":Y.TOT.RELZ.AMT:"*":Y.TOT.CM.POR
            !CRT Y.ISSUE.DATE:"*":Y.CATE.DES:"*":Y.PRE.LC.TYPE:"*":Y.TOT.COUNT:"*":Y.PRE.LC.CURR:"*":Y.TOT.LC.AMT:"*":Y.FROM.DATE:"*":Y.TO.DATE:"*":Y.CUS.NO:"*":Y.CUS.NAME:"*":Y.PRE.CATE:"*":Y.TOT.CM.POR:"*":Y.TOT.RELZ.AMT

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

        EB.DataAccess.FRead(FN.LC.TP.DES,Y.CATE,R.CATE,F.LC.TP.DES,Y.CATE.ERR)
*  Y.CATE.DES = R.CATE<LC.TYP.DESCRIPTION>
        Y.CATE.DES = R.CATE<LC.Config.Types.TypDescription>
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
            Y.TOT.CM.POR=Y.TOT.CM.POR+Y.CM.POR
            Y.TOT.RELZ.AMT=Y.TOT.RELZ.AMT+Y.RELZ.AMT
            Y.TOT.LC.AMT = Y.TOT.LC.AMT+Y.LC.DOC.AMT
            Y.TOT.COUNT=Y.TOT.COUNT+Y.COUNT

            GOSUB GET.LC.CATE.DES
            GOSUB GET.HIS.DATA

        END ELSE
            Y.CUR.LC.TYPE=Y.LC.TYPE
            Y.CUR.LC.CURR=Y.LC.CURR
            Y.CUR.CATE=Y.CATE

            IF (Y.PRE.CATE NE Y.CUR.CATE) OR (Y.PRE.LC.CURR NE Y.CUR.LC.CURR) THEN
                !IF (Y.PRE.CATE NE Y.CUR.CATE) OR (Y.PRE.LC.CURR NE Y.CUR.LC.CURR) OR (Y.ISS.DT NE Y.ISSUE.DATE) THEN

                *Y.RETURN<-1> =  Y.ISS.DT:"*":Y.CATE.DES:"*":Y.PRE.LC.TYPE:"*":Y.TOT.COUNT:"*":Y.PRE.LC.CURR:"*":Y.TOT.LC.AMT:"*":Y.FROM.DATE:"*":Y.TO.DATE:"*":Y.CUS.NO:"*":Y.CUS.NAME:"*":Y.PRE.CATE:"*":Y.TOT.RELZ.AMT:"*":Y.TOT.CM.POR
                !CRT Y.ISS.DT:"*":Y.CATE.DES:"*":Y.PRE.LC.TYPE:"*":Y.TOT.COUNT:"*":Y.PRE.LC.CURR:"*":Y.TOT.LC.AMT:"*":Y.FROM.DATE:"*":Y.TO.DATE:"*":Y.CUS.NO:"*":Y.CUS.NAME:"*":Y.PRE.CATE:"*":Y.TOT.CM.POR:"*":Y.TOT.RELZ.AMT

                Y.TOT.DRAW.AMT=''
                Y.DRAW.CURR=''
                Y.ISS.DT = Y.ISSUE.DATE
                Y.TOT.COUNT=''
                Y.TOT.COUNT.DR.AMT=''
                Y.PRE.CATE=Y.CATE
                Y.PRE.LC.TYPE = Y.LC.TYPE
                Y.PRE.LC.CURR = Y.LC.CURR
                Y.TOT.CM.POR = ''
                Y.TOT.RELZ.AMT = ''
                Y.TOT.LC.AMT = ''
                Y.TOT.LC.LIB.AMT = ''
                Y.TOT.CM.POR=Y.TOT.CM.POR+Y.CM.POR
                Y.TOT.RELZ.AMT=Y.TOT.RELZ.AMT+Y.RELZ.AMT
                Y.TOT.LC.AMT = Y.TOT.LC.AMT+Y.LC.DOC.AMT
                Y.TOT.COUNT=Y.TOT.COUNT+Y.COUNT

                GOSUB GET.LC.CATE.DES
                GOSUB GET.HIS.DATA

            END ELSE

                Y.ISS.DT = Y.ISSUE.DATE
                Y.PRE.CATE=Y.CATE
                Y.PRE.LC.TYPE = Y.LC.TYPE
                Y.PRE.LC.CURR = Y.LC.CURR
                Y.TOT.CM.POR=Y.TOT.CM.POR+Y.CM.POR
                Y.TOT.RELZ.AMT=Y.TOT.RELZ.AMT+Y.RELZ.AMT
                Y.TOT.LC.AMT = Y.TOT.LC.AMT+Y.LC.DOC.AMT
                Y.TOT.COUNT=Y.TOT.COUNT+Y.COUNT

                GOSUB GET.LC.CATE.DES

            END
        END
        RETURN
    END

*------------
GET.HIS.DATA:
*------------
    !DEBUG
    IF Y.CUS.NO EQ '' AND Y.FROM.DATE NE '' AND Y.TO.DATE NE '' THEN
        SEL.HIS.DATA ='SELECT ':FN.DR.HIS :' WITH (DRAWING.TYPE EQ ':"MA":' OR  DRAWING.TYPE EQ ':"SP":') AND DRAW.CURRENCY EQ ':Y.LC.CURR:' AND LC.CREDIT.TYPE EQ ':Y.LC.TYPE:' AND CO.CODE EQ ':Y.COMPANY:' AND LT.TF.DT.PAYMNT GE ':Y.FROM.DATE:' AND LT.TF.DT.PAYMNT LE ':Y.TO.DATE:' BY LC.CREDIT.TYPE BY DRAW.CURRENCY BY LT.TF.DT.PAYMNT BY-DSND @ID'
        EB.DataAccess.Readlist(SEL.HIS.DATA,SEL.LIST.HIS,'',NO.OF.HIS.REC,HIS.ERR)
    END ELSE
        IF Y.CUS.NO NE '' AND Y.FROM.DATE EQ '' AND Y.TO.DATE EQ '' THEN
            SEL.HIS.DATA ='SELECT ':FN.DR.HIS :' WITH  (DRAWING.TYPE EQ ':"MA":' OR  DRAWING.TYPE EQ ':"SP":') AND DRAW.CURRENCY EQ ':Y.LC.CURR:' AND LC.CREDIT.TYPE EQ ':Y.LC.TYPE:' AND CO.CODE EQ ':Y.COMPANY:' AND CUSTOMER.LINK EQ ':Y.CUS.NO:' BY LC.CREDIT.TYPE BY DRAW.CURRENCY BY LT.TF.DT.PAYMNT BY-DSND @ID'
            EB.DataAccess.Readlist(SEL.HIS.DATA,SEL.LIST.HIS,'',NO.OF.HIS.REC,HIS.ERR)
        END ELSE
            SEL.HIS.DATA ='SELECT ':FN.DR.HIS :' WITH (DRAWING.TYPE EQ ':"MA":' OR  DRAWING.TYPE EQ ':"SP":') AND DRAW.CURRENCY EQ ':Y.LC.CURR:' AND LC.CREDIT.TYPE EQ ':Y.LC.TYPE:' AND CO.CODE EQ ':Y.COMPANY:' AND LT.TF.DT.PAYMNT GE ':Y.FROM.DATE:' AND LT.TF.DT.PAYMNT LE ':Y.TO.DATE:' AND CUSTOMER.LINK EQ ':Y.CUS.NO:' BY LC.CREDIT.TYPE BY DRAW.CURRENCY BY LT.TF.DT.PAYMNT BY-DSND @ID'
            EB.DataAccess.Readlist(SEL.HIS.DATA,SEL.LIST.HIS,'',NO.OF.HIS.REC,HIS.ERR)
        END
    END
    LOOP
        REMOVE Y.LC.HIS.ID FROM SEL.LIST.HIS SETTING POS2
    WHILE Y.LC.HIS.ID : POS2
        EB.DataAccess.FRead(FN.DR.HIS,Y.LC.HIS.ID,R.LC.HIS,F.DR.HIS,Y.DRAW.HIS.ERR)

        FIND Y.LC.HIS.ID[1,14] IN Y.LIST SETTING POS1,POS2 THEN
        END
        ELSE
            IF Y.DRAW.HIS.ID NE Y.LC.HIS.ID[1,14] THEN

                Y.DRAW.HIS.ID=Y.LC.HIS.ID[1,14]
                Y.HIS.COUNT='1'
* Y.LC.DOC.AMT= R.LC.HIS<TF.DR.LOCAL.REF,136>
                APPLICATION.NAMES = 'DRAWINGS'
                LOCAL.FIELDS = 'LT.TF.CO.DOCAMT'
                EB.Foundation.MapLocalFields(APPLICATION.NAMES, LOCAL.FIELDS, FLD.POS)
                Y.ACPT.AMT.POS=FLD.POS<1,1>
                Y.ACPT.AMT = R.LC.HIS<LC.Contract.Drawings.TfDrLocalRef,Y.ACPT.AMT.POS>
*Y.CM.POR= R.LC.HIS<TF.DR.APP.DRAW.AMT>
                Y.CM.POR= R.LC.HIS<LC.Contract.Drawings.TfDrAppDrawAmt>
*Y.RELZ.AMT= R.LC.HIS<TF.DR.DOC.AMT.LC.CCY>
                Y.RELZ.AMT= R.LC.HIS<LC.Contract.Drawings.TfDrDocAmtLcCcy>
                Y.TOT.CM.POR=Y.TOT.CM.POR+Y.CM.POR
                Y.TOT.RELZ.AMT=Y.TOT.RELZ.AMT+Y.RELZ.AMT
                Y.TOT.LC.AMT = Y.TOT.LC.AMT+Y.LC.DOC.AMT
                Y.TOT.COUNT=Y.TOT.COUNT+Y.HIS.COUNT

            END
        END
    REPEAT
RETURN

*-----------------
GET.HIS.MAIN.DATA:
*-----------------
    !DEBUG
    IF Y.CUS.NO EQ '' AND Y.FROM.DATE NE '' AND Y.TO.DATE NE '' THEN
        SEL.HIS.DATA ='SELECT ':FN.DR.HIS :' WITH (DRAWING.TYPE EQ ':"MA":' OR  DRAWING.TYPE EQ ':"SP":') AND LC.CREDIT.TYPE EQ ':Y.TYPE:' AND CO.CODE EQ ':Y.COMPANY:' AND LT.TF.DT.PAYMNT GE ':Y.FROM.DATE:' AND LT.TF.DT.PAYMNT LE ':Y.TO.DATE:' BY LC.CREDIT.TYPE BY DRAW.CURRENCY BY LT.TF.DT.PAYMNT BY-DSND @ID'
        EB.DataAccess.Readlist(SEL.HIS.DATA,SEL.LIST.HIS,'',NO.OF.HIS.REC,HIS.ERR)
    END ELSE
        IF Y.CUS.NO NE '' AND Y.FROM.DATE EQ '' AND Y.TO.DATE EQ '' THEN
            SEL.HIS.DATA ='SELECT ':FN.DR.HIS :' WITH (DRAWING.TYPE EQ ':"MA":' OR  DRAWING.TYPE EQ ':"SP":') AND LC.CREDIT.TYPE EQ ':Y.TYPE:' AND CO.CODE EQ ':Y.COMPANY:' AND CON.CUS.LINK EQ ':Y.CUS.NO:' BY LC.CREDIT.TYPE BY DRAW.CURRENCY BY LT.TF.DT.PAYMNT BY-DSND @ID'
            EB.DataAccess.Readlist(SEL.HIS.DATA,SEL.LIST.HIS,'',NO.OF.HIS.REC,HIS.ERR)
        END ELSE
            SEL.HIS.DATA ='SELECT ':FN.DR.HIS :' WITH (DRAWING.TYPE EQ ':"MA":' OR  DRAWING.TYPE EQ ':"SP":') AND LC.CREDIT.TYPE EQ ':Y.TYPE:' AND CO.CODE EQ ':Y.COMPANY:' AND LT.TF.DT.PAYMNT GE ':Y.FROM.DATE:' AND LT.TF.DT.PAYMNT LE ':Y.TO.DATE:' AND CON.CUS.LINK EQ ':Y.CUS.NO:' BY LC.CREDIT.TYPE BY DRAW.CURRENCY BY LT.TF.DT.PAYMNT BY-DSND @ID'
            EB.DataAccess.Readlist(SEL.HIS.DATA,SEL.LIST.HIS,'',NO.OF.HIS.REC,HIS.ERR)
        END
    END
    LOOP
        REMOVE Y.LC.HIS.ID FROM SEL.LIST.HIS SETTING POS2
    WHILE Y.LC.HIS.ID : POS2
        EB.DataAccess.FRead(FN.DR.HIS,Y.LC.HIS.ID,R.DRAW.HIS,F.DR.HIS,Y.DRAW.HIS.ERR)

        IF Y.LC.NO NE Y.LC.HIS.ID[1,14] THEN
            Y.LC.NO = Y.LC.HIS.ID[1,14]
*Y.LC.TYPE = R.DRAW.HIS<TF.DR.LC.CREDIT.TYPE>
            Y.LC.TYPE = R.DRAW.HIS<LC.Contract.Drawings.TfDrLcCreditType>
*Y.LC.CURR = R.DRAW.HIS<TF.DR.DRAW.CURRENCY>
            Y.LC.CURR = R.DRAW.HIS<LC.Contract.Drawings.TfDrDrawCurrency>
* Y.LC.DOC.AMT = R.DRAW.HIS<TF.DR.DOCUMENT.AMOUNT>
            Y.LC.DOC.AMT = R.DRAW.HIS<LC.Contract.Drawings.TfDrDocumentAmount>
*  Y.RELZ.AMT =R.DRAW.HIS<TF.DR.DOC.AMT.LC.CCY>
            Y.RELZ.AMT = R.DRAW.HIS<LC.Contract.Drawings.TfDrDocAmtLcCcy>
* Y.ISSUE.DATE = R.DRAW.HIS<TF.DR.VALUE.DATE>
            Y.ISSUE.DATE = R.DRAW.HIS<LC.Contract.Drawings.TfDrValueDate>
*Y.CATE = R.DRAW.HIS<TF.DR.LC.CREDIT.TYPE>
            Y.CATE = R.DRAW.HIS<LC.Contract.Drawings.TfDrLcCreditType>
            !CALL GET.LOC.REF('DRAWINGS','IMPORTER.NAME',44)
* Y.CUS.NAME=R.LC<TF.DR.LOCAL.REF,44,1>
            APPLICATION.NAMES = 'DRAWINGS'
            LOCAL.FIELDS = 'LT.TF.IMPR.NAME'
            EB.Foundation.MapLocalFields(APPLICATION.NAMES, LOCAL.FIELDS, FLD.POS)
            Y.CUS.NAME.POS=FLD.POS<1,1>
            Y.CUS.NAME = R.DRAW.HIS<LC.Contract.Drawings.TfDrLocalRef,Y.CUS.NAME.POS>
            Y.COUNT='1'
            Y.COUNT.OUT.BAL='1'
            !END
            IF NO.OF.HIS.REC EQ '1' THEN

                Y.ISS.DT = Y.ISSUE.DATE

                GOSUB GET.LC.CATE.DES
                GOSUB GET.HIS.DATA

                Y.RETURN<-1> = Y.ISSUE.DATE:"*":Y.CATE.DES:"*":Y.LC.TYPE:"*":Y.COUNT:"*":Y.LC.CURR:"*":Y.LC.DOC.AMT:"*":Y.FROM.DATE:"*":Y.TO.DATE:"*":Y.CUS.NO:"*":Y.CUS.NAME:"*":Y.CATE:"*":Y.RELZ.AMT:"*":Y.CM.POR
                !CRT Y.ISSUE.DATE:"*":Y.CATE.DES:"*":Y.LC.TYPE:"*":Y.COUNT:"*":Y.LC.CURR:"*":Y.LC.DOC.AMT:"*":Y.FROM.DATE:"*":Y.TO.DATE:"*":Y.CUS.NO:"*":Y.CUS.NAME:"*":Y.CATE:"*":Y.TOT.CM.POR:"*":Y.TOT.RELZ.AMT

                Y.TOT.DRAW.AMT=''
                Y.DRAW.CURR=''
                Y.TOT.COUNT.DR.AMT=''
            END ELSE

                GOSUB GET.LC.HIS.TYPE

            END
        END

    !DEBUG
    IF NO.OF.HIS.REC NE '1' THEN

        Y.RETURN<-1> =  Y.ISSUE.DATE:"*":Y.CATE.DES:"*":Y.PRE.LC.TYPE:"*":Y.TOT.COUNT:"*":Y.PRE.LC.CURR:"*":Y.TOT.LC.AMT:"*":Y.FROM.DATE:"*":Y.TO.DATE:"*":Y.CUS.NO:"*":Y.CUS.NAME:"*":Y.PRE.CATE:"*":Y.TOT.RELZ.AMT:"*":Y.TOT.CM.POR
        !CRT Y.ISSUE.DATE:"*":Y.CATE.DES:"*":Y.PRE.LC.TYPE:"*":Y.TOT.COUNT:"*":Y.PRE.LC.CURR:"*":Y.TOT.LC.AMT:"*":Y.FROM.DATE:"*":Y.TO.DATE:"*":Y.CUS.NO:"*":Y.CUS.NAME:"*":Y.PRE.CATE:"*":Y.TOT.CM.POR:"*":Y.TOT.RELZ.AMT

        Y.TOT.DRAW.AMT=''
        Y.DRAW.CURR=''
        Y.TOT.COUNT.DR.AMT=''
    END
    ***outer loop end
    REPEAT
RETURN
!END
*---------------
GET.LC.HIS.TYPE:
*---------------
    !DEBUG
    IF Y.PRE.CATE EQ '' AND Y.CUR.CATE EQ '' THEN

        !IF Y.LC.NO.1 NE Y.LC.HIS.ID[1,14] THEN

        Y.LC.NO.1 = Y.LC.HIS.ID[1,14]
        Y.PRE.LC.TYPE=Y.LC.TYPE
        Y.PRE.LC.CURR=Y.LC.CURR
        Y.CUR.LC.TYPE=Y.LC.TYPE
        Y.CUR.LC.CURR=Y.LC.CURR

        Y.PRE.CATE=Y.CATE
        Y.CUR.CATE=Y.CATE

        Y.ISS.DT = Y.ISSUE.DATE
        Y.TOT.CM.POR=Y.TOT.CM.POR+Y.CM.POR
        Y.TOT.RELZ.AMT=Y.TOT.RELZ.AMT+Y.RELZ.AMT
        Y.TOT.LC.AMT = Y.TOT.LC.AMT+Y.LC.DOC.AMT
        Y.TOT.COUNT=Y.TOT.COUNT+Y.COUNT

        GOSUB GET.LC.CATE.DES
        !END
    END ELSE
        !IF Y.LC.NO.2 NE Y.LC.HIS.ID[1,14] THEN

        Y.LC.NO.2 =Y.LC.HIS.ID[1,14]
        Y.CUR.LC.TYPE=Y.LC.TYPE
        Y.CUR.LC.CURR=Y.LC.CURR
        Y.CUR.CATE=Y.CATE
        !END

        IF (Y.PRE.CATE NE Y.CUR.CATE) OR (Y.PRE.LC.CURR NE Y.CUR.LC.CURR) THEN
            !DEBUG
            !IF Y.LC.NO NE Y.LC.HIS.ID[1,14] THEN
            *Y.RETURN<-1> =  Y.ISS.DT:"*":Y.CATE.DES:"*":Y.PRE.LC.TYPE:"*":Y.TOT.COUNT:"*":Y.PRE.LC.CURR:"*":Y.TOT.LC.AMT:"*":Y.FROM.DATE:"*":Y.TO.DATE:"*":Y.CUS.NO:"*":Y.CUS.NAME:"*":Y.PRE.CATE:"*":Y.TOT.RELZ.AMT:"*":Y.TOT.CM.POR
            !CRT Y.ISS.DT:"*":Y.CATE.DES:"*":Y.PRE.LC.TYPE:"*":Y.TOT.COUNT:"*":Y.PRE.LC.CURR:"*":Y.TOT.LC.AMT:"*":Y.FROM.DATE:"*":Y.TO.DATE:"*":Y.CUS.NO:"*":Y.CUS.NAME:"*":Y.PRE.CATE:"*":Y.TOT.CM.POR:"*":Y.TOT.RELZ.AMT

            !IF Y.LC.NO.3 NE Y.LC.HIS.ID[1,14] THEN

            Y.LC.NO.3 =Y.LC.HIS.ID[1,14]
            Y.TOT.DRAW.AMT=''
            Y.DRAW.CURR=''
            Y.ISS.DT = Y.ISSUE.DATE
            Y.TOT.COUNT=''
            Y.TOT.COUNT.DR.AMT=''
            Y.PRE.CATE=Y.CATE
            Y.PRE.LC.TYPE = Y.LC.TYPE
            Y.PRE.LC.CURR = Y.LC.CURR
            Y.TOT.CM.POR = ''
            Y.TOT.RELZ.AMT = ''
            Y.TOT.LC.AMT = ''
            Y.TOT.LC.LIB.AMT = ''
            Y.TOT.CM.POR=Y.TOT.CM.POR+Y.CM.POR
            Y.TOT.RELZ.AMT=Y.TOT.RELZ.AMT+Y.RELZ.AMT
            Y.TOT.LC.AMT = Y.TOT.LC.AMT+Y.LC.DOC.AMT
            Y.TOT.COUNT=Y.TOT.COUNT+Y.COUNT

            GOSUB GET.LC.CATE.DES
            !END
        END ELSE
            !IF Y.LC.NO.4 NE Y.LC.HIS.ID[1,14] THEN

            Y.LC.NO.4 =Y.LC.HIS.ID[1,14]
            Y.ISS.DT = Y.ISSUE.DATE
            Y.PRE.CATE=Y.CATE
            Y.PRE.LC.TYPE = Y.LC.TYPE
            Y.PRE.LC.CURR = Y.LC.CURR
            Y.TOT.CM.POR=Y.TOT.CM.POR+Y.CM.POR
            Y.TOT.RELZ.AMT=Y.TOT.RELZ.AMT+Y.RELZ.AMT
            Y.TOT.LC.AMT = Y.TOT.LC.AMT+Y.LC.DOC.AMT
            Y.TOT.COUNT=Y.TOT.COUNT+Y.COUNT

            GOSUB GET.LC.CATE.DES
        END
    END
    !END
RETURN

FILE.WRITE:
    DEBUG
    WRITE.FILE.VAR = "print the line"

****************************

    Y.LOG.FILE='LimonTextFile.txt'
    Y.FILE.DIR ='./DFE.TEST'
    OPENSEQ Y.FILE.DIR,Y.LOG.FILE TO F.FILE.DIR ELSE NULL
    WRITESEQ WRITE.FILE.VAR APPEND TO F.FILE.DIR ELSE NULL
    CLOSESEQ F.FILE.DIR
RETURN

END
