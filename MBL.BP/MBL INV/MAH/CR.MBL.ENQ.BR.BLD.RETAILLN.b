* @ValidationCode : Mjo1NzMzMzE4NDpDcDEyNTI6MTYzNTY2MzgzNTg1Nzp1c2VyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMTcxMC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 31 Oct 2021 13:03:55
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

SUBROUTINE CR.MBL.ENQ.BR.BLD.RETAILLN(ENQ.DATA)

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON

    Y.FIELDS = ENQ.DATA<2>
    Y.POS = DCOUNT(Y.FIELDS,@VM) + 1
    ENQ.DATA<2,Y.POS> = 'PRODUCT.GROUP'
    ENQ.DATA<3,Y.POS> = 'EQ'
    ENQ.DATA<4,Y.POS> = 'MBL.ANY.PURP.LN MBL.AUTO.RET.LN MBL.COTT.RET.LN MBL.DOCT.RET.LN MBL.EDU.RET.LN MBL.HOUSE.FU.LN MBL.OTH.CONS.LN MBL.OVRS.EMP.LN MBL.PER.RET.LN MBL.PRANTIK.LN MBL.SML.SCH.LN'
 
RETURN
END