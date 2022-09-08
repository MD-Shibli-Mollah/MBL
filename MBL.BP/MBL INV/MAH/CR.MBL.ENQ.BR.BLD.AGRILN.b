* @ValidationCode : MjotMTU4ODU3MzEzNzpDcDEyNTI6MTYyNzM2Nzc1OTg1ODp1c2VyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMTcxMC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 27 Jul 2021 12:35:59
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

SUBROUTINE CR.MBL.ENQ.BR.BLD.AGRILN(ENQ.DATA)

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON

    Y.FIELDS = ENQ.DATA<2>
    Y.POS = DCOUNT(Y.FIELDS,@VM) + 1
    ENQ.DATA<2,Y.POS> = 'PRODUCT.GROUP'
    ENQ.DATA<3,Y.POS> = 'EQ'
    ENQ.DATA<4,Y.POS> = 'MBL.CSHCR.AG.LN MBL.SOD.AGR.LN MBL.LEAS.AGR.LN MBL.SHRT.AGR.LN MBL.TERM.AGR.LN'
 
RETURN
END