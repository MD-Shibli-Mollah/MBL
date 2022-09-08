* @ValidationCode : MjoxMTYzMzAwOTk1OkNwMTI1MjoxNjMxNTI2MDAwODAxOnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 13 Sep 2021 15:40:00
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

* @AUTHOR         : MD SHIBLI MOLLAH

SUBROUTINE CR.MBL.ENQ.BR.BLD.TERMLN(ENQ.DATA)

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON

    Y.FIELDS = ENQ.DATA<2>
    Y.POS = DCOUNT(Y.FIELDS,@VM) + 1
    ENQ.DATA<2,Y.POS> = 'PRODUCT.GROUP'
    ENQ.DATA<3,Y.POS> = 'EQ'
    ENQ.DATA<4,Y.POS> = 'MBL.TL.STIM.PACK.LN MBL.TL.AGRI.REF.LN MBL.TL.FSF.LN MBL.BB.RF.FC.LN MBL.GEN.CORP.LN MBL.HBL.LN MBL.HIRE.PUR.LN MBL.LEAS.FIN.LN MBL.TERM.FIS.LN MBL.TERM.LN MBL.TRM.NGO.LN'

RETURN
END