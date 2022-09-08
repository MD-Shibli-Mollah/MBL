* @ValidationCode : Mjo4MzMyOTI1MzA6Q3AxMjUyOjE1OTY3MDY0NzMxNTA6dXNlcjotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDE3MTAuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 06 Aug 2020 15:34:33
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
*
*-----------------------------------------------------------------------------
SUBROUTINE TF.MBL.E.NOF.LC.EXPORT(Y.RETURN)
*PROGRAM E.LC.EXPORT
    $INSERT  I_COMMON
    $INSERT  I_EQUATE
    $INSERT  I_ENQUIRY.COMMON
*  $INSERT  I_F.COMPANY
    $USING ST.CompanyCreation
* $INSERT  I_F.LETTER.OF.CREDIT
    $USING  LC.Contract
*$INSERT  I_F.LC.TYPES
    $USING LC.Config
* $INSERT  I_F.CATEGORY
    $USING EB.Utility
*$INSERT  I_F.DRAWINGS
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
RETURN
*---------
INIT:
*---------

    FN.LC ='F.LETTER.OF.CREDIT'
    F.LC =''

    FN.LC.TP.DES = 'F.LC.TYPES'
    F.LC.TP.DES =''

    FN.LC.HIS ='F.LETTER.OF.CREDIT$HIS'
    F.LC.HIS =''

    FN.DR ='F.DRAWINGS'
    F.DR =''

    FN.DR.HIS='F.DRAWINGS$HIS'
    F.DR.HIS =''

    Y.TYPE ='CONT EFDU EFSC EFDT EFST EAUL EDCF ESCF ESCL EACL EDUF ESTC ESTF EMC EMU  EMTC EMTU'
    !Y.TYPE = 'EFSC'
    !Y.FOREIGN= 'CONT EFDU EFSC EFDT EFST'
    !Y.INLAND='EAUL EDCF ESCF ESCL EACL EDUF ESTC ESTF'
    !Y.MIXED='EMC EMU  EMTC EMTU'
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
    EB.DataAccess.Opf(FN.LC.HIS,F.LC.HIS)
    EB.DataAccess.Opf(FN.DR,F.DR)
    EB.DataAccess.Opf(FN.DR.HIS,F.DR.HIS)
RETURN

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
        Y.CUS.NO= EB.Reports.getEnqSelection()<4,CUS.POS>
    END
    
    Y.COMPANY = EB.SystemTables.getIdCompany()

    IF  Y.TO.DATE = "" THEN
        Y.TO.DATE = Y.FROM.DATE
    END
    !DEBUG
    IF Y.CUS.NO EQ '' AND Y.FROM.DATE NE '' AND Y.TO.DATE NE '' THEN
        SEL.CMD ='SELECT ':FN.DR :' WITH LC.CREDIT.TYPE EQ ':Y.TYPE:' AND CO.CODE EQ ':Y.COMPANY:' AND LT.TF.DT.DOCREC GE ':Y.FROM.DATE:' AND LT.TF.DT.DOCREC LE ':Y.TO.DATE:' BY LC.CREDIT.TYPE BY DRAW.CURRENCY BY LT.TF.DT.DOCREC'
        EB.DataAccess.Readlist(SEL.CMD,SEL.LIST,'',NO.OF.REC,RET.CODE)
    END
    ELSE IF Y.CUS.NO NE '' AND Y.FROM.DATE EQ '' AND Y.TO.DATE EQ '' THEN
        SEL.CMD ='SELECT ':FN.DR :' WITH LC.CREDIT.TYPE EQ ':Y.TYPE:' AND CO.CODE EQ ':Y.COMPANY:' AND LT.TF.APL.CUSNO EQ ':Y.CUS.NO:' BY LC.CREDIT.TYPE BY DRAW.CURRENCY BY LT.TF.DT.DOCREC'
        EB.DataAccess.Readlist(SEL.CMD,SEL.LIST,'',NO.OF.REC,RET.CODE)
    END
    ELSE
        SEL.CMD ='SELECT ':FN.DR :' WITH LC.CREDIT.TYPE EQ ':Y.TYPE:' AND CO.CODE EQ ':Y.COMPANY:' BY LC.CREDIT.TYPE BY DRAW.CURRENCY BY LT.TF.DT.DOCREC'
        EB.DataAccess.Readlist(SEL.CMD,SEL.LIST,'',NO.OF.REC,RET.CODE)
    END
*DEBUG
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
            EB.DataAccess.FRead(FN.DR,Y.LC.ID,R.LC,F.DR,Y.ERR)

            Y.LC.NO = Y.LC.ID
* Y.LC.TYPE = R.LC<TF.DR.LC.CREDIT.TYPE>
            Y.LC.TYPE =R.LC<LC.Contract.Drawings.TfDrLcCreditType>
* Y.LC.CURR = R.LC<TF.DR.DRAW.CURRENCY>
            Y.LC.CURR =R.LC<LC.Contract.Drawings.TfDrDrawCurrency>
* Y.LC.DOC.AMT = R.LC<TF.DR.DOCUMENT.AMOUNT>
            Y.LC.DOC.AMT = R.LC<LC.Contract.Drawings.TfDrDocumentAmount>
           
* Y.ACPT.AMT = R.LC<TF.DR.LOCAL.REF,136>
            APPLICATION.NAMES = 'DRAWINGS'
            LOCAL.FIELDS = 'LT.TF.CO.DOCAMT'
            EB.Foundation.MapLocalFields(APPLICATION.NAMES, LOCAL.FIELDS, FLD.POS)
            
            Y.ACPT.AMT.POS=FLD.POS<1,1>
            Y.ACPT.AMT = R.LC<LC.Contract.Drawings.TfDrLocalRef,Y.ACPT.AMT.POS>
                               
*Y.RELZ.AMT =R.LC<TF.DR.DOC.AMT.LC.CCY>
            Y.RELZ.AMT =R.LC<LC.Contract.Drawings.TfDrAssnAmtLcCcy>
* Y.ISSUE.DATE = R.LC<TF.DR.VALUE.DATE>
            Y.ISSUE.DATE = R.LC<LC.Contract.Drawings.TfDrValueDate>
* Y.CATE = R.LC<TF.DR.LC.CREDIT.TYPE>
            Y.CATE = R.LC<LC.Contract.Drawings.TfDrLcCreditType>
            
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

                IF Y.ACPT.AMT EQ '' THEN
                    Y.RETURN<-1> = Y.ISSUE.DATE:"*":Y.CATE.DES:"*":Y.LC.TYPE:"*":Y.COUNT:"*":Y.LC.CURR:"*":Y.LC.DOC.AMT:"*":Y.FROM.DATE:"*":Y.TO.DATE:"*":Y.CUS.NO:"*":Y.CUS.NAME:"*":Y.CATE
                    !CRT Y.ISSUE.DATE:"*":Y.CATE.DES:"*":Y.LC.TYPE:"*":Y.COUNT:"*":Y.LC.CURR:"*":Y.LC.DOC.AMT:"*":Y.FROM.DATE:"*":Y.TO.DATE:"*":Y.CUS.NO:"*":Y.CUS.NAME
                END
                ELSE
                    Y.RETURN<-1> = Y.ISSUE.DATE:"*":Y.CATE.DES:"*":Y.LC.TYPE:"*":Y.COUNT:"*":Y.LC.CURR:"*":Y.ACPT.AMT:"*":Y.FROM.DATE:"*":Y.TO.DATE:"*":Y.CUS.NO:"*":Y.CUS.NAME:"*":Y.CATE
                    !CRT Y.ISSUE.DATE:"*":Y.CATE.DES:"*":Y.LC.TYPE:"*":Y.COUNT:"*":Y.LC.CURR:"*":Y.ACPT.AMT:"*":Y.FROM.DATE:"*":Y.TO.DATE:"*":Y.CUS.NO:"*":Y.CUS.NAME
                END
                Y.TOT.DRAW.AMT=''
                Y.DRAW.CURR=''
                Y.TOT.COUNT.DR.AMT=''
            END ELSE

                GOSUB GET.LC.TYPE

            END


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
        EB.DataAccess.FRead(FN.LC.TP.DES,Y.CATE,R.CATE,F.LC.TP.DES,Y.CATE.ERR)
*Y.CATE.DES = R.CATE<LC.TYP.DESCRIPTION>
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
            Y.TOT.COUNT=Y.TOT.COUNT+Y.COUNT

            IF Y.ACPT.AMT EQ '' THEN
                Y.TOT.LC.AMT = Y.TOT.LC.AMT+Y.LC.DOC.AMT
            END
            ELSE
                Y.TOT.LC.AMT = Y.TOT.LC.AMT+Y.ACPT.AMT
            END

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

                IF Y.ACPT.AMT EQ '' THEN
                    Y.TOT.LC.AMT = Y.TOT.LC.AMT+Y.LC.DOC.AMT
                END
                ELSE
                    Y.TOT.LC.AMT = Y.TOT.LC.AMT+Y.ACPT.AMT
                END

                GOSUB GET.LC.CATE.DES
                GOSUB GET.HIS.DATA

            END ELSE

                Y.ISS.DT = Y.ISSUE.DATE
                Y.TOT.COUNT=Y.TOT.COUNT+Y.COUNT

                Y.PRE.CATE=Y.CATE

                Y.PRE.LC.TYPE = Y.LC.TYPE
                Y.PRE.LC.CURR = Y.LC.CURR

                IF Y.ACPT.AMT EQ '' THEN
                    Y.TOT.LC.AMT = Y.TOT.LC.AMT+Y.LC.DOC.AMT
                END
                ELSE
                    Y.TOT.LC.AMT = Y.TOT.LC.AMT+Y.ACPT.AMT
                END
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
        SEL.HIS.DATA ='SELECT ':FN.DR.HIS :' WITH RECORD.STATUS EQ ':"MAT":' AND DRAW.CURRENCY EQ ':Y.LC.CURR:' AND LC.CREDIT.TYPE EQ ':Y.LC.TYPE:' AND CO.CODE EQ ':Y.COMPANY:' AND LT.TF.DT.DOCREC GE ':Y.FROM.DATE:' AND LT.TF.DT.DOCREC LE ':Y.TO.DATE:' BY LC.CREDIT.TYPE BY DRAW.CURRENCY BY LT.TF.DT.DOCREC'
        EB.DataAccess.Readlist(SEL.HIS.DATA,SEL.LIST.HIS,'',NO.OF.HIS.REC,HIS.ERR)
    END
    ELSE IF Y.CUS.NO NE '' AND Y.FROM.DATE EQ '' AND Y.TO.DATE EQ '' THEN
        SEL.HIS.DATA ='SELECT ':FN.DR.HIS :' WITH RECORD.STATUS EQ ':"MAT":' AND DRAW.CURRENCY EQ ':Y.LC.CURR:' AND LC.CREDIT.TYPE EQ ':Y.LC.TYPE:' AND CO.CODE EQ ':Y.COMPANY:' AND LT.TF.APL.CUSNO EQ ':Y.CUS.NO:' BY LC.CREDIT.TYPE BY DRAW.CURRENCY BY LT.TF.DT.DOCREC'
        EB.DataAccess.Readlist(SEL.HIS.DATA,SEL.LIST.HIS,'',NO.OF.HIS.REC,HIS.ERR)
    END
    ELSE
        SEL.HIS.DATA ='SELECT ':FN.DR.HIS :' WITH RECORD.STATUS EQ ':"MAT":' AND DRAW.CURRENCY EQ ':Y.LC.CURR:' AND LC.CREDIT.TYPE EQ ':Y.LC.TYPE:' AND CO.CODE EQ ':Y.COMPANY:' BY LC.CREDIT.TYPE BY DRAW.CURRENCY BY LT.TF.DT.DOCREC'
        EB.DataAccess.Readlist(SEL.HIS.DATA,SEL.LIST.HIS,'',NO.OF.HIS.REC,HIS.ERR)
    END
    LOOP
        REMOVE Y.LC.HIS.ID FROM SEL.LIST.HIS SETTING POS2
    WHILE Y.LC.HIS.ID : POS2
        EB.DataAccess.FRead(FN.DR.HIS,Y.LC.HIS.ID,R.LC.HIS,F.DR.HIS,Y.DRAW.HIS.ERR)

        FIND Y.LC.HIS.ID[1,14] IN Y.LIST SETTING POS1,POS2 THEN
        END
        ELSE
            IF Y.DRAW.HIS.ID NE Y.LC.HIS.ID[1,14] THEN

                Y.HIS.COUNT='1'
*Y.LC.DOC.AMT = R.LC.HIS<TF.DR.DOCUMENT.AMOUNT>
                Y.LC.DOC.AMT = R.LC.HIS<LC.Contract.Drawings.TfDrDocumentAmount>
                
*Y.ACPT.AMT = R.LC.HIS<TF.DR.LOCAL.REF,136>
                APPLICATION.NAMES = 'DRAWINGS'
                LOCAL.FIELDS = 'LT.TF.CO.DOCAMT'
                EB.Foundation.MapLocalFields(APPLICATION.NAMES, LOCAL.FIELDS, FLD.POS)
            
                Y.ACPT.AMT.POS=FLD.POS<1,1>
                Y.ACPT.AMT = R.LC.HIS<LC.Contract.Drawings.TfDrLocalRef,Y.ACPT.AMT.POS>
                                                          
                Y.DRAW.HIS.ID=Y.LC.HIS.ID[1,14]
                !Y.TOT.LC.AMT=Y.TOT.LC.AMT+Y.DRAW.HIS.AMT

                IF Y.ACPT.AMT EQ '' THEN
                    Y.TOT.LC.AMT = Y.TOT.LC.AMT+Y.LC.DOC.AMT
                END
                ELSE
                    Y.TOT.LC.AMT = Y.TOT.LC.AMT+Y.ACPT.AMT
                END
                Y.TOT.COUNT=Y.TOT.COUNT+Y.HIS.COUNT
            END
        END
    REPEAT
RETURN

*-----------------
GET.HIS.MAIN.DATA:
*-----------------
    IF Y.CUS.NO EQ '' AND Y.FROM.DATE NE '' AND Y.TO.DATE NE '' THEN
        SEL.HIS.DATA ='SELECT ':FN.DR.HIS :' WITH RECORD.STATUS EQ ':"MAT":' AND LC.CREDIT.TYPE EQ ':Y.TYPE:' AND CO.CODE EQ ':Y.COMPANY:' AND LT.TF.DT.DOCREC GE ':Y.FROM.DATE:' AND LT.TF.DT.DOCREC LE ':Y.TO.DATE:' BY LC.CREDIT.TYPE BY LC.CURRENCY BY LT.TF.DT.DOCREC'
        EB.DataAccess.Readlist(SEL.HIS.DATA,SEL.LIST.HIS,'',NO.OF.HIS.REC,HIS.ERR)
    END
    ELSE IF Y.CUS.NO NE '' AND Y.FROM.DATE EQ '' AND Y.TO.DATE EQ '' THEN
        SEL.HIS.DATA ='SELECT ':FN.DR.HIS :' WITH RECORD.STATUS EQ ':"MAT":' AND LC.CREDIT.TYPE EQ ':Y.TYPE:' AND CO.CODE EQ ':Y.COMPANY:' AND CON.CUS.LINK EQ ':Y.CUS.NO:' BY LC.CREDIT.TYPE BY LC.CURRENCY BY LT.TF.DT.DOCREC'
        EB.DataAccess.Readlist(SEL.HIS.DATA,SEL.LIST.HIS,'',NO.OF.HIS.REC,HIS.ERR)
    END
    ELSE
        SEL.HIS.DATA ='SELECT ':FN.DR.HIS :' WITH RECORD.STATUS EQ ':"MAT":' AND LC.CREDIT.TYPE EQ ':Y.TYPE:' AND CO.CODE EQ ':Y.COMPANY:' BY LC.CREDIT.TYPE BY LC.CURRENCY BY LT.TF.DT.DOCREC'
        EB.DataAccess.Readlist(SEL.HIS.DATA,SEL.LIST.HIS,'',NO.OF.HIS.REC,HIS.ERR)
    END
    LOOP
        REMOVE Y.LC.HIS.ID FROM SEL.LIST.HIS SETTING POS2
    WHILE Y.LC.HIS.ID : POS2
        EB.DataAccess.FRead(FN.DR.HIS,Y.LC.HIS.ID,R.DRAW.HIS,F.DR.HIS,Y.DRAW.HIS.ERR)

        Y.LC.NO = Y.LC.ID
* Y.LC.TYPE = R.DRAW.HIS<TF.DR.LC.CREDIT.TYPE>
        Y.LC.TYPE = R.DRAW.HIS<LC.Contract.Drawings.TfDrLcCreditType>
* Y.LC.CURR = R.DRAW.HIS<TF.DR.DRAW.CURRENCY>
        Y.LC.CURR = R.DRAW.HIS<LC.Contract.Drawings.TfDrDrawCurrency>
*Y.LC.DOC.AMT = R.DRAW.HIS<TF.DR.DOCUMENT.AMOUNT>
        Y.LC.DOC.AMT = R.DRAW.HIS<LC.Contract.Drawings.TfDrDocumentAmount>

*Y.ACPT.AMT = R.DRAW.HIS<TF.DR.LOCAL.REF,136>
        
        APPLICATION.NAMES = 'DRAWINGS'
        LOCAL.FIELDS = 'LT.TF.CO.DOCAMT'
        EB.Foundation.MapLocalFields(APPLICATION.NAMES, LOCAL.FIELDS, FLD.POS)
        Y.ACPT.AMT.POS=FLD.POS<1,1>
        Y.ACPT.AMT = R.DRAW.HIS<LC.Contract.Drawings.TfDrLocalRef,Y.ACPT.AMT.POS>
                                    
* Y.RELZ.AMT = R.DRAW.HIS<TF.DR.DOC.AMT.LC.CCY>
        Y.RELZ.AMT = R.DRAW.HIS<LC.Contract.Drawings.TfDrDocAmtLcCcy>
*Y.ISSUE.DATE = R.DRAW.HIS<TF.DR.VALUE.DATE>
        Y.ISSUE.DATE = R.DRAW.HIS<LC.Contract.Drawings.TfDrValueDate>
        !Y.CATE = R.DRAW.HIS<TF.DR.LC.CREDIT.TYPE>
        Y.CATE = R.DRAW.HIS<LC.Contract.Drawings.TfDrLcCreditType>

*  Y.CUS.NAME=R.DRAW.HIS<TF.DR.LOCAL.REF,44,1>
        APPLICATION.NAMES = 'DRAWINGS'
        LOCAL.FIELDS = 'LT.TF.IMPR.NAME'
        EB.Foundation.MapLocalFields(APPLICATION.NAMES, LOCAL.FIELDS, FLD.POS)
        Y.CUS.NAME.POS=FLD.POS<1,1>
        Y.CUS.NAME = R.DRAW.HIS<LC.Contract.Drawings.TfDrLocalRef,Y.CUS.NAME.POS>
                
        Y.COUNT='1'
        Y.COUNT.OUT.BAL='1'

        IF NO.OF.HIS.REC EQ '1' THEN
            !GOSUB GET.LC.TP.DET

            Y.ISS.DT = Y.ISSUE.DATE

            GOSUB GET.LC.CATE.DES
            GOSUB GET.HIS.DATA

            IF Y.ACPT.AMT EQ '' THEN
                Y.RETURN<-1> = Y.ISSUE.DATE:"*":Y.CATE.DES:"*":Y.LC.TYPE:"*":Y.COUNT:"*":Y.LC.CURR:"*":Y.LC.DOC.AMT:"*":Y.FROM.DATE:"*":Y.TO.DATE:"*":Y.CUS.NO:"*":Y.CUS.NAME:"*":Y.CATE
                !CRT Y.ISSUE.DATE:"*":Y.CATE.DES:"*":Y.LC.TYPE:"*":Y.COUNT:"*":Y.LC.CURR:"*":Y.LC.DOC.AMT:"*":Y.FROM.DATE:"*":Y.TO.DATE:"*":Y.CUS.NO:"*":Y.CUS.NAME
            END
            ELSE
                Y.RETURN<-1> = Y.ISSUE.DATE:"*":Y.CATE.DES:"*":Y.LC.TYPE:"*":Y.COUNT:"*":Y.LC.CURR:"*":Y.ACPT.AMT:"*":Y.FROM.DATE:"*":Y.TO.DATE:"*":Y.CUS.NO:"*":Y.CUS.NAME:"*":Y.CATE
                !CRT Y.ISSUE.DATE:"*":Y.CATE.DES:"*":Y.LC.TYPE:"*":Y.COUNT:"*":Y.LC.CURR:"*":Y.ACPT.AMT:"*":Y.FROM.DATE:"*":Y.TO.DATE:"*":Y.CUS.NO:"*":Y.CUS.NAME
            END
            Y.TOT.DRAW.AMT=''
            Y.DRAW.CURR=''
            Y.TOT.COUNT.DR.AMT=''
        END
        ELSE

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

        IF Y.ACPT.AMT EQ '' THEN
            Y.TOT.LC.AMT = Y.TOT.LC.AMT+Y.LC.DOC.AMT
        END
        ELSE
            Y.TOT.LC.AMT = Y.TOT.LC.AMT+Y.ACPT.AMT
        END
        !Y.TOT.LC.AMT = Y.TOT.LC.AMT+Y.LC.AMT

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

            IF Y.ACPT.AMT EQ '' THEN
                Y.TOT.LC.AMT = Y.TOT.LC.AMT+Y.LC.DOC.AMT
            END
            ELSE
                Y.TOT.LC.AMT = Y.TOT.LC.AMT+Y.ACPT.AMT
            END
            !Y.TOT.LC.AMT = Y.TOT.LC.AMT+Y.LC.AMT

            GOSUB GET.LC.CATE.DES
            !GOSUB GET.HIS.DATA

        END ELSE

            Y.ISS.DT = Y.ISSUE.DATE
            Y.TOT.COUNT=Y.TOT.COUNT+Y.COUNT

            Y.PRE.CATE=Y.CATE

            Y.PRE.LC.TYPE = Y.LC.TYPE
            Y.PRE.LC.CURR = Y.LC.CURR

            IF Y.ACPT.AMT EQ '' THEN
                Y.TOT.LC.AMT = Y.TOT.LC.AMT+Y.LC.DOC.AMT
            END
            ELSE
                Y.TOT.LC.AMT = Y.TOT.LC.AMT+Y.ACPT.AMT
            END
            !Y.TOT.LC.AMT = Y.TOT.LC.AMT+Y.LC.AMT

            GOSUB GET.LC.CATE.DES

        END
    END
RETURN
END