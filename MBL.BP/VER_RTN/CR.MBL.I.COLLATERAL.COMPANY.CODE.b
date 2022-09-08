SUBROUTINE CR.MBL.I.COLLATERAL.COMPANY.CODE
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
*
    $USING CO.Contract
    $USING EB.DataAccess
    $USING EB.SystemTables
    $USING EB.Foundation
*
    GOSUB INIT
    GOSUB OPENFILE
    GOSUB PROCESS
*
RETURN
*-----------------------------------------------------------------------------
*-------
INIT:
*-------
    FN.COLL = 'F.COLLATERAL'
    F.COLL = ''
*
    FN.COLL.RIGHT = 'F.COLLATERAL.RIGHT'
    F.COLL.RIGHT = ''
RETURN
*---------
OPENFILE:
*---------
    EB.DataAccess.Opf(FN.COLL,F.COLL)
    EB.DataAccess.Opf(FN.COLL.RIGHT,F.FN.COLL.RIGHT)
RETURN
*---------
PROCESS:
*---------
    IF EB.SystemTables.getApplication() EQ 'COLLATERAL.RIGHT' THEN
*
        IF EB.SystemTables.getVFunction() EQ 'R' THEN
            Y.COM.CODE = EB.SystemTables.getIdCompany()
            EB.Foundation.MapLocalFields("COLLATERAL.RIGHT","LT.CHQ.COM.CODE",Y.B.POS)
            Y.LOCAL.NAME=EB.SystemTables.getRNew(CO.Contract.CollateralRight.CollRightLocalRef)
            Y.LOCAL.NAME<1,Y.B.POS> = Y.COM.CODE
            EB.SystemTables.setRNew(CO.Contract.CollateralRight.CollRightLocalRef, Y.LOCAL.NAME)
        END
        IF EB.SystemTables.getVFunction() EQ 'H' THEN
            Y.COM.CODE = EB.SystemTables.getIdCompany()
            EB.Foundation.MapLocalFields("COLLATERAL.RIGHT","LT.CHQ.COM.CODE",Y.B.POS)
            Y.LOCAL.NAME=EB.SystemTables.getRNew(CO.Contract.CollateralRight.CollRightLocalRef)
            Y.LOCAL.NAME<1,Y.B.POS> = Y.COM.CODE
            EB.SystemTables.setRNew(CO.Contract.CollateralRight.CollRightLocalRef, Y.LOCAL.NAME)
        END
    END
*
    IF EB.SystemTables.getApplication() EQ 'COLLATERAL' THEN
        IF EB.SystemTables.getVFunction() EQ 'R' THEN
            Y.COM.CODE = EB.SystemTables.getIdCompany()
            EB.Foundation.MapLocalFields("COLLATERAL.RIGHT","LT.CHQ.COM.CODE",Y.B.POS)
            Y.LOCAL.NAME=EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)
            Y.LOCAL.NAME<1,Y.B.POS> = Y.COM.CODE
            EB.SystemTables.setRNew(CO.Contract.Collateral.CollLocalRef , Y.LOCAL.NAME)
        END
        IF EB.SystemTables.getVFunction() EQ 'H' THEN
            Y.COM.CODE = EB.SystemTables.getIdCompany()
            EB.Foundation.MapLocalFields("COLLATERAL.RIGHT","LT.CHQ.COM.CODE",Y.B.POS)
            Y.LOCAL.NAME=EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)
            Y.LOCAL.NAME<1,Y.B.POS> = Y.COM.CODE
            EB.SystemTables.setRNew(CO.Contract.Collateral.CollLocalRef , Y.LOCAL.NAME)
        END
    END
RETURN
END