* @ValidationCode : MjotNjMyODI0MDQ4OkNwMTI1MjoxNjI5Mjk0OTkwMjA4OnVzZXI6LTE6LTE6MDowOnRydWU6Ti9BOkRFVl8yMDE3MTAuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 18 Aug 2021 19:56:30
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : true
* @ValidationInfo : Compiler Version  : DEV_201710.0
* <Rating>-7</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE MBL.CUS.PRIV.TABLE.FIELDS
*-----------------------------------------------------------------------------
*<doc>
* Template for field definitions routine MBL.CUS.PRIV.TABLE.FIELDS
*
* @author smollah@fortress-global.com
* @stereotype fields template
* @uses Table
* @public Table Creation
* @package infra.eb
* </doc>
*-----------------------------------------------------------------------------
* Modification History :
*
* 17TH AUG 2021 - MD Shibli Mollah
*            Fortress Data Services
*
*-----------------------------------------------------------------------------
*** <region name= Header>
*** <desc>Inserts and control logic</desc>
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_DataTypes
*** </region>
*-----------------------------------------------------------------------------
    CALL Table.defineId("MBL.CUS.PRV.ID", T24_String) ;* Define Table id
*-----------------------------------------------------------------------------
    
    CALL Table.addFieldDefinition("TT.OD.SELLING.SPREAD", "35", "A","") ;* Add a new field
    CALL Table.addField("TT.CLEAN.SPREAD", T24_String, "", "")
    
*-----------------------------------------------------------------------------
*RESERVED FIELDS
*-----------------------------------------------------------------------------
    CALL Table.addReservedField('RESERVED.05')
    CALL Table.addReservedField('RESERVED.04')
    CALL Table.addReservedField('RESERVED.03')
    CALL Table.addReservedField('RESERVED.02')
    CALL Table.addReservedField('RESERVED.01')
      
*-----------------------------------------------------------------------------
    CALL Table.setAuditPosition ;* Poputale audit information
*-----------------------------------------------------------------------------
RETURN
*-----------------------------------------------------------------------------
END
