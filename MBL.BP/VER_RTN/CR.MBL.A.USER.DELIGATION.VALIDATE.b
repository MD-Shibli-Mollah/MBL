SUBROUTINE CR.MBL.A.USER.DELIGATION.VALIDATE
*-----------------------------------------------------------------------------
*
*Subroutine Description: This routine attached in Limit Application for User Limit permision.
*Subroutine Type       : Calculation
*Attached To           : Version Control or Version
*Attached As           :
*Developed by          : Kamrul
*Incoming Parameters   :
*Outgoing Parameters   :
*-----------------------------------------------------------------------------
*** <region name= Arguments>
*** <desc>To define the arguments </desc>
* Incoming Arguments:
*

*
* Outgoing Arguments:
*
* balanceAmount - Bonus amount calculated will be passed
*
*** </region>
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.EB.CR.MBL.BMB.USER.DELIGATION
*
    $USING ST.CompanyCreation
    $USING EB.Security
    $USING LI.Config
    $USING EB.Updates
    $USING EB.DataAccess
    $USING EB.LocalReferences
    $USING EB.SystemTables
    $USING EB.ErrorProcessing
*
    GOSUB INITIALISE
    GOSUB PROCESS
*
RETURN
*----------
INITIALISE:
*----------
*
* This section is to initialise all the applications and variables.
*
    FN.EB.BMB.USER.DELIGATION = 'F.EB.CR.MBL.BMB.USER.DELIGATION'
    F.EB.BMB.USER.DELIGATION = ''
    EB.DataAccess.Opf(FN.EB.BMB.USER.DELIGATION,F.EB.BMB.USER.DELIGATION)
*
    FN.LIMIT='F.LIMIT'
    F.LIMIT=''
    EB.DataAccess.Opf(FN.LIMIT,F.LIMIT)
*
    Y.APP = 'LIMIT'
    Y.FLD = 'LT.LM.SUB.PARNT' : VM : 'LT.LM.CHILD.LMT'
    Y.POS = ''
    EB.Updates.MultiGetLocRef(Y.APP,Y.FLD,Y.POS)
    Y.SUB.PARENT.POS = Y.POS<1,1>
    Y.CHILD.LIMIT.POS = Y.POS<1,2>
*
    Y.USER.ID = EB.SystemTables.getOperator()
    Y.SUB.PARENT = EB.SystemTables.getRNew(LI.Config.Limit.LocalRef)<1,Y.SUB.PARENT.POS>
    Y.CHILD.LIMIT = EB.SystemTables.getRNew(LI.Config.Limit.LocalRef)<1,Y.CHILD.LIMIT.POS>
*
*******CHANGE ON 20160128 FOR PAD***********************
    IF Y.SUB.PARENT EQ '' THEN
        Y.SUB.PARENT =EB.SystemTables.getRNew(LI.Config.Limit.LimitProduct)
    END
*******CHANGE ON 20160128 FOR PAD**********************
*
    Y.INTERNAL.AMOUNT = EB.SystemTables.getRNew(LI.Config.Limit.InternalAmount)
    Y.OLD.INT.AMOUNT = EB.SystemTables.getROld(LI.Config.Limit.InternalAmount)
    Y.LIABILITY.NUMBER=EB.SystemTables.getRNew(LI.Config.Limit.LiabilityNumber)
    Y.NEW.DATE=EB.SystemTables.getRNew(LI.Config.Limit.ExpiryDate)
    Y.OLD.DATE=EB.SystemTables.getROld(LI.Config.Limit.ExpiryDate)
    Y.REC.STATUS=EB.SystemTables.getRNew(LI.Config.Limit.CurrNo)
    Y.STATUS=EB.SystemTables.getRNew(LI.Config.Limit.RecordStatus)
    IF Y.CHILD.LIMIT ELSE
        Y.CHILD.LIMIT = Y.SUB.PARENT
    END
    Y.LIMIT = ''
    Y.AMT = ''
    Y.LIMIT=EB.SystemTables.getIdNew()
RETURN
*-------
PROCESS:
*-------
* This section is to define actual process and validations.
*
*******CHANGE ON 20160218 FOR REVERSE TXN NOT TO CHECK**********
*
    IF Y.STATUS NE 'RNAU' THEN
*
*******CHANGE ON 20160218 FOR REVERSE TXN NOT TO CHECK***********
*
        EB.DataAccess.FRead(FN.EB.BMB.USER.DELIGATION,Y.USER.ID,R.LIMIT.REC,F.EB.BMB.USER.DELIGATION,Y.LIMIT.ERR)
        IF R.LIMIT.REC THEN
            Y.LIMIT = R.LIMIT.REC<EB.BMB53.CHILD.LIMIT>
            Y.AMT = R.LIMIT.REC<EB.BMB53.INTERNAL.AMOUNT>
            LOCATE Y.CHILD.LIMIT IN Y.LIMIT<1,1> SETTING Y.LIMIT.POS THEN
                Y.INT.AMT = Y.AMT<1,Y.LIMIT.POS>
*
                SEL.CMD= "SELECT ":FN.LIMIT:' WITH @ID MATCHES ':Y.LIABILITY.NUMBER:'.':'000':Y.CHILD.LIMIT:'...'
                EB.DataAccess.Readlist(SEL.CMD,SEL.LIST,'',NO.OF.REC,ERR.CODE)
                LOOP
                    REMOVE Y.LIMIT.NO FROM SEL.LIST SETTING Y.POS
                WHILE Y.LIMIT.NO:Y.POS
                    EB.DataAccess.FRead(FN.LIMIT,Y.LIMIT.NO,R.LIMIT,F.LIMIT,NO.ERR)
                    Y.LIMIT.AMT=R.LIMIT<LI.Config.Limit.InternalAmount>
                    Y.LIMIT.AMT1+=Y.LIMIT.AMT
                REPEAT
*
                IF Y.REC.STATUS EQ '1' THEN
                    Y.TOTAL.AMT=Y.LIMIT.AMT1+Y.INTERNAL.AMOUNT
                END ELSE
                    IF Y.INTERNAL.AMOUNT GT Y.OLD.INT.AMOUNT THEN
                        Y.TOTAL.AMT=Y.LIMIT.AMT1+(Y.INTERNAL.AMOUNT-Y.OLD.INT.AMOUNT)
                    END ELSE
                        Y.TOTAL.AMT=Y.LIMIT.AMT1
                    END
                END
                IF Y.TOTAL.AMT GT Y.INT.AMT AND EB.SystemTables.getVFunction() EQ 'A' THEN
                    EB.SystemTables.setEtext('Authorisation not allowed')
                    EB.ErrorProcessing.StoreEndError()
                    RETURN
                END
            END ELSE IF EB.SystemTables.getVFunction() EQ 'A' THEN
                EB.SystemTables.setEtext('Authorisation not allowed')
                EB.ErrorProcessing.StoreEndError()
                RETURN
            END
            RETURN
        END
    END
