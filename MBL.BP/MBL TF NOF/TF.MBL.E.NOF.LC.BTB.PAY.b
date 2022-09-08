* @ValidationCode : MjoxNTQ3MTMzOTgwOkNwMTI1MjoxNTk2NzA2ODk2NzQzOnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 06 Aug 2020 15:41:36
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
SUBROUTINE TF.MBL.E.NOF.LC.BTB.PAY(Y.RETURN)
*PROGRAM TF.MBL.E.NOF.LC.BTB.PAY
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
* $INSERT I_F.COMPANY
    $USING ST.CompanyCreation
*  $INSERT I_F.LC.TYPES
    $USING LC.Config
*  $INSERT I_F.CATEGORY
    $USING EB.Utility
*    $INSERT I_F.DRAWINGS
    $USING  LC.Contract
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
    FN.LC.TYPE = 'F.LC.TYPES'
    F.LC.TYPE =''

    FN.DRAW = 'F.DRAWINGS'
    F.DRAW =''

    FN.DRAW.HIS = 'FBNK.DRAWINGS$HIS'
    F.DRAW.HIS =''

    Y.TYPE ='BACL BDCF BFDC BFEU BFMC BFMU BIEC BMCF BMCL BSCF BSCL BSNE BSNF BSNI BSNL BSNT BSNZ BSPE BSPF BSPI BSPL BSPT BSPZ BSUF BUAE BUAF BUAI BUAL BUAT BUAZ BUNE BUNF BUNI BUNL BUNT BUNZ BZDC BZSC BZSE'
    !Y.TYPE = 'BFDC'
    Y.DR.PER.LC.TYPE =""
    Y.DR.CUR.LC.TYPE =""
    Y.PRE.LC.TYPE =""
    Y.CUR.LC.TYPE =""
    Y.PRE.CATE =""
    Y.CUR.CATE =""
    Y.FROM.DATE =''
    Y.TO.DATE =''
    Y.JUL.FR.DT=''
    Y.JUL.TO.DT=''
    Y.CUS.NO =''
RETURN
*----------
OPENFILES:
*----------
    EB.DataAccess.Opf(FN.LC.TYPE,F.LC.TYPE)
    EB.DataAccess.Opf(FN.DRAW,F.DRAW)
    EB.DataAccess.Opf(FN.DRAW.HIS,F.DRAW.HIS)
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
        Y.CUS.NO = EB.Reports.getEnqSelection()<4,CUS.POS>
    END
    
    Y.COMPANY = EB.SystemTables.getIdCompany()

    IF  Y.TO.DATE = "" THEN
        Y.TO.DATE = Y.FROM.DATE
    END
  
    !DEBUG
    IF Y.CUS.NO EQ '' AND Y.FROM.DATE NE '' AND Y.TO.DATE NE '' THEN
        SEL.CMD.DR.AMT ='SELECT ':FN.DRAW :' WITH LC.CREDIT.TYPE EQ ':Y.TYPE:' AND CO.CODE EQ ':Y.COMPANY:' AND LT.TF.DT.PAYMNT GE ':Y.FROM.DATE:' AND LT.TF.DT.PAYMNT LE ':Y.TO.DATE:' AND (DRAWING.TYPE EQ ':"MA":' OR  DRAWING.TYPE EQ ':"SP":') BY LC.CREDIT.TYPE BY DRAW.CURRENCY BY LT.TF.DT.PAYMNT'
        EB.DataAccess.Readlist(SEL.CMD.DR.AMT,SEL.LIST.DR.AMT,'',NO.OF.REC.DR.AMT,RET.CODE)
    END
    ELSE IF Y.CUS.NO NE '' AND Y.FROM.DATE EQ '' AND Y.TO.DATE EQ '' THEN
        SEL.CMD.DR.AMT ='SELECT ':FN.DRAW :' WITH LC.CREDIT.TYPE EQ ':Y.TYPE:' AND CO.CODE EQ ':Y.COMPANY:' AND CUSTOMER.LINK EQ ':Y.CUS.NO:' AND (DRAWING.TYPE EQ ':"MA":' OR  DRAWING.TYPE EQ ':"SP":') BY LC.CREDIT.TYPE BY DRAW.CURRENCY BY LT.TF.DT.PAYMNT'
        EB.DataAccess.Readlist(SEL.CMD.DR.AMT,SEL.LIST.DR.AMT,'',NO.OF.REC.DR.AMT,RET.CODE)
    END
    ELSE
        SEL.CMD.DR.AMT ='SELECT ':FN.DRAW :' WITH LC.CREDIT.TYPE EQ ':Y.TYPE:' AND CO.CODE EQ ':Y.COMPANY:' AND (DRAWING.TYPE EQ ':"MA":' OR  DRAWING.TYPE EQ ':"SP":') BY LC.CREDIT.TYPE BY DRAW.CURRENCY BY LT.TF.DT.PAYMNT'
        EB.DataAccess.Readlist(SEL.CMD.DR.AMT,SEL.LIST.DR.AMT,'',NO.OF.REC.DR.AMT,RET.CODE)
    END
    !DEBUG
    !Y.CNT=0
    IF NO.OF.REC.DR.AMT EQ 0 THEN
        GOSUB GET.HIS.MAIN.DATA
    END ELSE
        LOOP
            REMOVE Y.LC.ID.DR.AMT FROM SEL.LIST.DR.AMT SETTING POS
        WHILE Y.LC.ID.DR.AMT : POS

***outerloop start
            !Y.CNT++
            EB.DataAccess.FRead(FN.DRAW,Y.LC.ID.DR.AMT,R.DRAW,F.DRAW,Y.DRAW.ERR)

            !DEBUG
*Y.DRAW.TYPE = R.DRAW<TF.DR.LC.CREDIT.TYPE>
            Y.DRAW.TYPE =R.DRAW<LC.Contract.Drawings.TfDrLcCreditType>
* Y.DRAW.AMT = R.DRAW<TF.DR.DOCUMENT.AMOUNT>
            Y.DRAW.AMT = R.DRAW<LC.Contract.Drawings.TfDrDocumentAmount>
* Y.DRAW.CURR = R.DRAW<TF.DR.DRAW.CURRENCY>
            Y.DRAW.CURR =R.DRAW<LC.Contract.Drawings.TfDrDrawCurrency>

*  CALL GET.LOC.REF('DRAWINGS','IMPORTER.NAME',44)
            APPLICATION.NAMES = 'DRAWINGS'
            LOCAL.FIELDS = 'LT.TF.IMPR.NAME'
            EB.Foundation.MapLocalFields(APPLICATION.NAMES, LOCAL.FIELDS, FLD.POS)
            
            Y.IMP.NAME.POS=FLD.POS<1,1>
            Y.IMP.NAME = R.DRAW<LC.Contract.Drawings.TfDrLocalRef,Y.IMP.NAME.POS>
            
            
            
            
*Y.IMP.NAME=R.DRAW<TF.DR.LOCAL.REF,44,1>
            !Y.TOT.IMP.NAME=DCOUNT(Y.IMP.NAME,@SM)
            Y.COUNT='1'

            IF NO.OF.REC.DR.AMT EQ '1' THEN

                GOSUB GET.LC.CATE.DES

                IF NO.OF.REC.DR.AMT NE 0 THEN
                    GOSUB GET.HIS.DATA
                END

                Y.RETURN<-1> = Y.CATE.DES:"*":Y.DRAW.TYPE:"*":Y.COUNT:"*":Y.DRAW.CURR:"*":Y.DRAW.AMT:"*":Y.FROM.DATE:"*":Y.TO.DATE:"*":Y.CUS.NO:"*":Y.IMP.NAME
                !CRT Y.CATE.DES:"*":Y.DRAW.TYPE:"*":Y.COUNT:"*":Y.DRAW.CURR:"*":Y.DRAW.AMT:"*":Y.FROM.DATE:"*":Y.TO.DATE:"*":Y.CUS.NO:"*":Y.IMP.NAME
                Y.TOT.DRAW.AMT=''
                Y.DRAW.CURR=''
                Y.HIS.COUNT=''
                Y.TOT.COUNT.DR.AMT=''
            END ELSE

                GOSUB GET.DEBIT.AMOUNT

            END

            !DEBUG
            IF NO.OF.REC.DR.AMT NE '1' THEN

                Y.RETURN<-1> =  Y.CATE.DES:"*":Y.DR.PRE.LC.TYPE:"*":Y.TOT.COUNT:"*":Y.DR.PRE.LC.CURR:"*":Y.TOT.DRAW.AMT:"*":Y.FROM.DATE:"*":Y.TO.DATE:"*":Y.CUS.NO:"*":Y.IMP.NAME
                !CRT Y.CATE.DES:"*":Y.DR.PRE.LC.TYPE:"*":Y.TOT.COUNT:"*":Y.DR.PRE.LC.CURR:"*":Y.TOT.DRAW.AMT:"*":Y.FROM.DATE:"*":Y.TO.DATE:"*":Y.CUS.NO:"*":Y.IMP.NAME
                Y.TOT.DRAW.AMT=''
                Y.DRAW.CURR=''
                Y.TOT.COUNT.DR.AMT=''
            END
***outer loop end
        REPEAT
        RETURN
*----------------
GET.DEBIT.AMOUNT:
*----------------
        !DEBUG
        IF Y.DR.PRE.LC.TYPE EQ '' AND Y.DR.CUR.LC.TYPE EQ '' THEN

            Y.DR.PRE.LC.TYPE=Y.DRAW.TYPE
            Y.DR.PRE.LC.CURR=Y.DRAW.CURR
            Y.DR.CUR.LC.TYPE=Y.DRAW.TYPE
            Y.DR.CUR.LC.CURR=Y.DRAW.CURR
            Y.TOT.COUNT=Y.TOT.COUNT+Y.COUNT
            Y.TOT.DRAW.AMT = Y.TOT.DRAW.AMT+Y.DRAW.AMT

            GOSUB GET.LC.CATE.DES

            IF NO.OF.REC.DR.AMT NE 0 THEN
                GOSUB GET.HIS.DATA
            END

        END ELSE
            Y.DR.CUR.LC.TYPE=Y.DRAW.TYPE
            Y.DR.CUR.LC.CURR=Y.DRAW.CURR

            IF (Y.DR.PRE.LC.TYPE NE Y.DR.CUR.LC.TYPE) OR (Y.DR.PRE.LC.CURR NE Y.DR.CUR.LC.CURR) THEN

*Y.RETURN<-1> = Y.CATE.DES:"*":Y.DR.PRE.LC.TYPE:"*":Y.TOT.COUNT:"*":Y.DR.PRE.LC.CURR:"*":Y.TOT.DRAW.AMT:"*":Y.FROM.DATE:"*":Y.TO.DATE:"*":Y.CUS.NO:"*":Y.IMP.NAME
                !CRT Y.CATE.DES:"*":Y.DR.PRE.LC.TYPE:"*":Y.TOT.COUNT:"*":Y.DR.PRE.LC.CURR:"*":Y.TOT.DRAW.AMT:"*":Y.FROM.DATE:"*":Y.TO.DATE:"*":Y.CUS.NO:"*":Y.IMP.NAME

                Y.CNT=''
                Y.TOT.DRAW.AMT=''
                Y.TOT.COUNT=''
                Y.HIS.COUNT=''
                Y.TOT.COUNT=Y.TOT.COUNT+Y.COUNT

                Y.DR.PRE.LC.TYPE = Y.DRAW.TYPE
                Y.DR.PRE.LC.CURR = Y.DRAW.CURR
                Y.TOT.DRAW.AMT = Y.TOT.DRAW.AMT+Y.DRAW.AMT

                GOSUB GET.LC.CATE.DES

                IF NO.OF.REC.DR.AMT NE 0 THEN
                    GOSUB GET.HIS.DATA
                END

            END ELSE

                Y.DR.PRE.LC.TYPE = Y.DRAW.TYPE
                Y.DR.PRE.LC.CURR = Y.DRAW.CURR
                Y.TOT.DRAW.AMT = Y.TOT.DRAW.AMT+Y.DRAW.AMT
                Y.TOT.COUNT=Y.TOT.COUNT+Y.COUNT

                GOSUB GET.LC.CATE.DES

            END
        END
        RETURN
    END
*----------------
GET.LC.CATE.DES:
*----------------
    !DEBUG
    EB.DataAccess.FRead(FN.LC.TYPE,Y.DRAW.TYPE,R.CATE,F.LC.TYPE,Y.TYPE.ERR)
*Y.CATE.DES = R.CATE<LC.TYP.DESCRIPTION>
    Y.CATE.DES = R.CATE<LC.Config.Types.TypDescription>
RETURN

*------------
GET.HIS.DATA:
*------------
    !DEBUG
    IF Y.CUS.NO EQ '' AND Y.FROM.DATE NE '' AND Y.TO.DATE NE '' THEN
        SEL.HIS.DATA ='SELECT ':FN.DRAW.HIS :' WITH DRAW.CURRENCY EQ ':Y.DRAW.CURR:' AND LC.CREDIT.TYPE EQ ':Y.DRAW.TYPE:' AND CO.CODE EQ ':Y.COMPANY:' AND DATE.OF.PAYMENT GE ':Y.FROM.DATE:' AND DATE.OF.PAYMENT LE ':Y.TO.DATE:' AND (DRAWING.TYPE EQ ':"MA":' OR  DRAWING.TYPE EQ ':"SP":') BY LC.CREDIT.TYPE BY DRAW.CURRENCY BY DATE.OF.PAYMENT BY-DSND @ID'
        EB.DataAccess.Readlist(SEL.HIS.DATA,SEL.LIST.HIS,'',NO.OF.HIS.REC,HIS.ERR)
    END
    ELSE IF Y.CUS.NO NE '' AND Y.FROM.DATE EQ '' AND Y.TO.DATE EQ '' THEN
        SEL.HIS.DATA ='SELECT ':FN.DRAW.HIS :' WITH DRAW.CURRENCY EQ ':Y.DRAW.CURR:' AND LC.CREDIT.TYPE EQ ':Y.DRAW.TYPE:' AND CO.CODE EQ ':Y.COMPANY:' AND CUSTOMER.LINK EQ ':Y.CUS.NO:' AND (DRAWING.TYPE EQ ':"MA":' OR  DRAWING.TYPE EQ ':"SP":') BY LC.CREDIT.TYPE BY DRAW.CURRENCY BY DATE.OF.PAYMENT BY-DSND @ID'
        EB.DataAccess.Readlist(SEL.HIS.DATA,SEL.LIST.HIS,'',NO.OF.HIS.REC,HIS.ERR)
    END
    ELSE
        SEL.HIS.DATA ='SELECT ':FN.DRAW.HIS :' WITH DRAW.CURRENCY EQ ':Y.DRAW.CURR:' AND LC.CREDIT.TYPE EQ ':Y.DRAW.TYPE:' AND CO.CODE EQ ':Y.COMPANY:' AND (DRAWING.TYPE EQ ':"MA":' OR  DRAWING.TYPE EQ ':"SP":') BY LC.CREDIT.TYPE BY DRAW.CURRENCY BY DATE.OF.PAYMENT BY-DSND @ID'
        EB.DataAccess.Readlist(SEL.HIS.DATA,SEL.LIST.HIS,'',NO.OF.HIS.REC,HIS.ERR)
    END
    LOOP
        REMOVE Y.LC.HIS.ID FROM SEL.LIST.HIS SETTING POS2
    WHILE Y.LC.HIS.ID : POS2
        EB.DataAccess.FRead(FN.DRAW.HIS,Y.LC.HIS.ID,R.DRAW.HIS,F.DRAW.HIS,Y.DRAW.HIS.ERR)
        Y.LIST=''
        FIND Y.LC.HIS.ID[1,14] IN Y.LIST SETTING POS1,POS2 THEN
        END
        ELSE
            IF Y.DRAW.HIS.ID NE Y.LC.HIS.ID[1,14] THEN

                Y.HIS.COUNT='1'
* Y.DRAW.HIS.AMT = R.DRAW.HIS<TF.DR.DOCUMENT.AMOUNT>
                Y.DRAW.HIS.AMT = R.DRAW.HIS<LC.Contract.Drawings.TfDrDocumentAmount>
                Y.DRAW.HIS.ID=Y.LC.HIS.ID[1,14]
                Y.TOT.DRAW.AMT=Y.TOT.DRAW.AMT+Y.DRAW.HIS.AMT
                Y.TOT.COUNT=Y.TOT.COUNT+Y.HIS.COUNT
            END
        END
    REPEAT
RETURN
*-----------------
GET.HIS.MAIN.DATA:
*-----------------
    IF Y.CUS.NO EQ '' AND Y.FROM.DATE NE '' AND Y.TO.DATE NE '' THEN
        SEL.HIS.DATA ='SELECT ':FN.DRAW.HIS :' WITH LC.CREDIT.TYPE EQ ':Y.TYPE:' AND CO.CODE EQ ':Y.COMPANY:' AND MATURITY.DATE GE ':Y.FROM.DATE:' AND MATURITY.DATE. LE ':Y.TO.DATE:' AND (DRAWING.TYPE EQ ':"MA":' OR  DRAWING.TYPE EQ ':"SP":') BY LC.CREDIT.TYPE BY DRAW.CURRENCY BY MATURITY.DATE BY-DSND @ID'
        EB.DataAccess.Readlist(SEL.HIS.DATA,SEL.LIST.HIS,'',NO.OF.HIS.REC,HIS.ERR)
    END
    ELSE IF Y.CUS.NO NE '' AND Y.FROM.DATE EQ '' AND Y.TO.DATE EQ '' THEN
        SEL.HIS.DATA ='SELECT ':FN.DRAW.HIS :' WITH LC.CREDIT.TYPE EQ ':Y.TYPE:' AND CO.CODE EQ ':Y.COMPANY:' AND CUSTOMER.LINK EQ ':Y.CUS.NO:' AND (DRAWING.TYPE EQ ':"MA":' OR  DRAWING.TYPE EQ ':"SP":') BY LC.CREDIT.TYPE BY DRAW.CURRENCY BY MATURITY.DATE BY-DSND @ID'
        EB.DataAccess.Readlist(SEL.HIS.DATA,SEL.LIST.HIS,'',NO.OF.HIS.REC,HIS.ERR)
    END
    ELSE
        SEL.HIS.DATA ='SELECT ':FN.DRAW.HIS :' WITH LC.CREDIT.TYPE EQ ':Y.TYPE:' AND CO.CODE EQ ':Y.COMPANY:' AND (DRAWINGS.TYPE EQ ':"MA":' OR  DRAWING.TYPE EQ ':"SP":') BY LC.CREDIT.TYPE BY DRAW.CURRENCY BY MATURITY.DATE BY-DSND @ID'
        EB.DataAccess.Readlist(SEL.HIS.DATA,SEL.LIST.HIS,'',NO.OF.HIS.REC,HIS.ERR)
    END
    LOOP
        REMOVE Y.LC.HIS.ID FROM SEL.LIST.HIS SETTING POS2
    WHILE Y.LC.HIS.ID : POS2
        EB.DataAccess.FRead(FN.DRAW.HIS,Y.LC.HIS.ID,R.DRAW.HIS,F.DRAW.HIS,Y.DRAW.HIS.ERR)

        IF Y.LC.NO NE Y.LC.HIS.ID[1,14] THEN
            Y.LC.NO = Y.LC.HIS.ID[1,14]
*Y.DRAW.TYPE = R.DRAW.HIS<TF.DR.LC.CREDIT.TYPE>
            Y.DRAW.TYPE = R.DRAW.HIS<LC.Contract.Drawings.TfDrLcCreditType>
*Y.DRAW.AMT = R.DRAW.HIS<TF.DR.DOCUMENT.AMOUNT>
            Y.DRAW.AMT = R.DRAW.HIS<LC.Contract.Drawings.TfDrDocumentAmount>
* Y.DRAW.CURR = R.DRAW.HIS<TF.DR.DRAW.CURRENCY>
            Y.DRAW.CURR = R.DRAW.HIS<LC.Contract.Drawings.TfDrDrawCurrency>

* CALL GET.LOC.REF('DRAWINGS','IMPORTER.NAME',44)
* Y.IMP.NAME=R.DRAW.HIS<TF.DR.LOCAL.REF,44,1>

            APPLICATION.NAMES = 'DRAWINGS'
            LOCAL.FIELDS = 'LT.TF.IMPR.NAME'
            EB.Foundation.MapLocalFields(APPLICATION.NAMES, LOCAL.FIELDS, FLD.POS)
            Y.IMP.NAME.POS=FLD.POS<1,1>
            Y.IMP.NAME = R.DRAW<LC.Contract.Drawings.TfDrLocalRef,Y.IMP.NAME.POS>

            Y.COUNT='1'

            IF NO.OF.HIS.REC EQ '1' THEN

                GOSUB GET.LC.CATE.DES

                Y.RETURN<-1> = Y.CATE.DES:"*":Y.DRAW.TYPE:"*":Y.COUNT:"*":Y.DRAW.CURR:"*":Y.DRAW.AMT:"*":Y.FROM.DATE:"*":Y.TO.DATE:"*":Y.CUS.NO:"*":Y.IMP.NAME
                !CRT Y.CATE.DES:"*":Y.DRAW.TYPE:"*":Y.COUNT:"*":Y.DRAW.CURR:"*":Y.DRAW.AMT:"*":Y.FROM.DATE:"*":Y.TO.DATE:"*":Y.CUS.NO:"*":Y.IMP.NAME
                Y.TOT.DRAW.AMT=''
                Y.DRAW.CURR=''
                Y.HIS.COUNT=''
                Y.TOT.COUNT.DR.AMT=''
            END ELSE

                GOSUB GET.DEBIT.HIS.AMOUNT
            END
        END


        IF NO.OF.HIS.REC NE '1' THEN

            Y.RETURN<-1> =  Y.CATE.DES:"*":Y.DR.PRE.LC.TYPE:"*":Y.TOT.COUNT:"*":Y.DR.PRE.LC.CURR:"*":Y.TOT.DRAW.AMT:"*":Y.FROM.DATE:"*":Y.TO.DATE:"*":Y.CUS.NO:"*":Y.IMP.NAME
            !CRT Y.CATE.DES:"*":Y.DR.PRE.LC.TYPE:"*":Y.TOT.COUNT:"*":Y.DR.PRE.LC.CURR:"*":Y.TOT.DRAW.AMT:"*":Y.FROM.DATE:"*":Y.TO.DATE:"*":Y.CUS.NO:"*":Y.IMP.NAME
            Y.TOT.DRAW.AMT=''
            Y.DRAW.CURR=''
            Y.TOT.COUNT.DR.AMT=''
        END
***outer loop end
    REPEAT
RETURN
*--------------------
GET.DEBIT.HIS.AMOUNT:
*--------------------
    !DEBUG
    IF Y.DR.PRE.LC.TYPE EQ '' AND Y.DR.CUR.LC.TYPE EQ '' THEN

        Y.DR.PRE.LC.TYPE=Y.DRAW.TYPE
        Y.DR.PRE.LC.CURR=Y.DRAW.CURR
        Y.DR.CUR.LC.TYPE=Y.DRAW.TYPE
        Y.DR.CUR.LC.CURR=Y.DRAW.CURR
        Y.TOT.COUNT=Y.TOT.COUNT+Y.COUNT
        Y.TOT.DRAW.AMT = Y.TOT.DRAW.AMT+Y.DRAW.AMT

        GOSUB GET.LC.CATE.DES

    END ELSE
        Y.DR.CUR.LC.TYPE=Y.DRAW.TYPE
        Y.DR.CUR.LC.CURR=Y.DRAW.CURR

        IF (Y.DR.PRE.LC.TYPE NE Y.DR.CUR.LC.TYPE) OR (Y.DR.PRE.LC.CURR NE Y.DR.CUR.LC.CURR) THEN

*Y.RETURN<-1> = Y.CATE.DES:"*":Y.DR.PRE.LC.TYPE:"*":Y.TOT.COUNT:"*":Y.DR.PRE.LC.CURR:"*":Y.TOT.DRAW.AMT:"*":Y.FROM.DATE:"*":Y.TO.DATE:"*":Y.CUS.NO:"*":Y.IMP.NAME
            !CRT Y.CATE.DES:"*":Y.DR.PRE.LC.TYPE:"*":Y.TOT.COUNT:"*":Y.DR.PRE.LC.CURR:"*":Y.TOT.DRAW.AMT:"*":Y.FROM.DATE:"*":Y.TO.DATE:"*":Y.CUS.NO:"*":Y.IMP.NAME

            Y.CNT=''
            Y.TOT.DRAW.AMT=''
            Y.TOT.COUNT=''
            Y.HIS.COUNT=''
            Y.TOT.COUNT=Y.TOT.COUNT+Y.COUNT

            Y.DR.PRE.LC.TYPE = Y.DRAW.TYPE
            Y.DR.PRE.LC.CURR = Y.DRAW.CURR
            Y.TOT.DRAW.AMT = Y.TOT.DRAW.AMT+Y.DRAW.AMT

            GOSUB GET.LC.CATE.DES

        END ELSE

            Y.DR.PRE.LC.TYPE = Y.DRAW.TYPE
            Y.DR.PRE.LC.CURR = Y.DRAW.CURR
            Y.TOT.DRAW.AMT = Y.TOT.DRAW.AMT+Y.DRAW.AMT
            Y.TOT.COUNT=Y.TOT.COUNT+Y.COUNT

            GOSUB GET.LC.CATE.DES

        END
    END
RETURN
END