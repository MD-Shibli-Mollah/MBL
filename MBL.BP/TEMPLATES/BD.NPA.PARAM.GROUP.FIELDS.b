* @ValidationCode : MjotMTAxNzM4MjQxNjpDcDEyNTI6MTYwMjY3OTM1NDUwMzp1c2VyOi0xOi0xOjA6MDp0cnVlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 14 Oct 2020 18:42:34
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
SUBROUTINE BD.NPA.PARAM.GROUP.FIELDS
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
    
    $USING AA.ProductFramework
    $USING EB.SystemTables
*** </region>
*-----------------------------------------------------------------------------
    CALL Table.defineId("BD.NPA.GR.ID", T24_String) ;* Define Table id
    
    EB.SystemTables.setIdCheckfile('BD.NPA.LN.TYPE')
    EB.SystemTables.setIdEnri(BD.NPA.LN.DESCRIPTION)
*-----------------------------------------------------------------------------
    CALL Table.addFieldDefinition("XX.PRODUCT.GROUP", "35", "A","") ;* Add a new field
    
    CALL Field.setCheckFile('AA.PRODUCT.GROUP':FM:AA.ProductFramework.ProductGroup.PgDescription:FM:'L')
     
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