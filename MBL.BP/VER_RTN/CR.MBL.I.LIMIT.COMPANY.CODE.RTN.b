SUBROUTINE CR.MBL.I.LIMIT.COMPANY.CODE.RTN

    $INSERT  I_COMMON
    $INSERT  I_EQUATE
    $INSERT  I_ENQUIRY.COMMON
    $USING  LI.Config
    
    $USING EB.DataAccess
    $USING EB.LocalReferences
    $USING EB.SystemTables
    $USING EB.ErrorProcessing

    FN.LIMIT='F.LIMIT'
    F.LIMIT=''
    EB.DataAccess.Opf(FN.LIMIT,F.LIMIT)

    IF EB.SystemTables.getVFunction() = 'R' OR EB.SystemTables.getVFunction() = 'H'  THEN
        Y.COM.CODE = EB.SystemTables.getIdCompany()
        EB.LocalReferences.GetLocRef("LIMIT","LT.CHQ.COM.CODE",Y.B.POS)
        Y.LOCAL.POS = EB.SystemTables.getRNew(LI.Config.Limit.LocalRef)
        Y.LOCAL.POS<1,Y.B.POS>=Y.COM.CODE
        EB.SystemTables.setRNew(LI.Config.Limit.LocalRef, Y.LOCAL.POS)
    END


RETURN
END