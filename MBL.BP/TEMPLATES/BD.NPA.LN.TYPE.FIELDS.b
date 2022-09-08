* @ValidationCode : MjotMTE3Mzk2OTc0MDpDcDEyNTI6MTYwMjY3NzA0MzY5NDp1c2VyOi0xOi0xOjA6MDp0cnVlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 14 Oct 2020 18:04:03
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : true
* @ValidationInfo : Compiler Version  : DEV_201710.0
*-----------------------------------------------------------------------------
* <Rating>-7</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE BD.NPA.LN.TYPE.FIELDS
*-----------------------------------------------------------------------------
*<doc>
* Template for field definitions routine EXIM.SMS.TRN.FIELDS
*
* @author MD SHIBLI MOLLAH
* @stereotype fields template
* @uses Table
* @public Table Creation
* @package infra.eb
* </doc>
*-----------------------------------------------------------------------------
* Modification History :
*
* 2020/10/14 - EN_10003543
*            New Template changes*
*-----------------------------------------------------------------------------
*** <region name= Header>
*** <desc>Inserts and control logic</desc>
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_DataTypes
*** </region>
*-----------------------------------------------------------------------------
    CALL Table.defineId("BD.NPA.LN.ID", T24_String) ;* Define Table id
*-----------------------------------------------------------------------------
    CALL Table.addFieldDefinition("DESCRIPTION", "35", "A","") ;* Add a new field
    
    
    CALL Table.addReservedField("RESERVED.10")
    CALL Table.addReservedField("RESERVED.09")
    CALL Table.addReservedField("RESERVED.08")
    CALL Table.addReservedField("RESERVED.07")
    CALL Table.addReservedField("RESERVED.06")
    CALL Table.addReservedField("RESERVED.05")
    CALL Table.addReservedField("RESERVED.04")
    CALL Table.addReservedField("RESERVED.03")
    CALL Table.addReservedField("RESERVED.02")
    CALL Table.addReservedField("RESERVED.01")
    
    CALL Table.addLocalReferenceField('XX.LOCAL.REF')
    CALL Table.addOverrideField
*-----------------------------------------------------------------------------
    CALL Table.setAuditPosition ;* Poputale audit information
*-----------------------------------------------------------------------------
RETURN
*-----------------------------------------------------------------------------
END
