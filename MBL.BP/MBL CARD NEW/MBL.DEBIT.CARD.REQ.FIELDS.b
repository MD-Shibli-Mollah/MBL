* @ValidationCode : MjoxNTM5OTg4MjI3OkNwMTI1MjoxNjMyNzQ5NDcxMTExOnVzZXI6LTE6LTE6MDowOnRydWU6Ti9BOkRFVl8yMDE3MTAuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 27 Sep 2021 19:31:11
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
SUBROUTINE MBL.DEBIT.CARD.REQ.FIELDS
*-----------------------------------------------------------------------------
*<doc>
* Template for field definitions routine MBL.DEBIT.CARD.REQ.FIELDS
*
* @author tcoleman@temenos.com
* @stereotype fields template
* @uses Table
* @public Table Creation
* @package infra.eb
* </doc>
*-----------------------------------------------------------------------------
* Modification History :
*
* 27 SEP 2021 - MD SHIBLI MOLLAH
*            New Template changes
*-----------------------------------------------------------------------------
*** <region name= Header>
*** <desc>Inserts and control logic</desc>
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_DataTypes
*** </region>

*-----------------------------------------------------------------------------
    CALL Table.defineId("CARD.ID", T24_Account) ;* Define Table id
    ID.CHECKFILE ='ACCOUNT'
*-----------------------------------------------------------------------------
    CALL Table.addOptionsField("CARD.REQUEST", "YES", "", "")
    CALL Table.addFieldDefinition("DATE", "11", "D", "")
    CALL Table.addFieldDefinition("MARKETED.BY", "35", "A", "")
    CALL Table.addFieldDefinition("CARD.CUSTOMER", "35", "A", "")
     
    CALL Table.addReservedField('RESERVED.05')
    CALL Table.addReservedField('RESERVED.04')
    CALL Table.addReservedField('RESERVED.03')
    CALL Table.addReservedField('RESERVED.02')
    CALL Table.addReservedField('RESERVED.01')
    CALL Table.addField('XX.LOCAL.REF', T24_String, Field_NoInput,'')
    CALL Table.addOverrideField
*-----------------------------------------------------------------------------
    CALL Table.setAuditPosition ;* Poputale audit information
*-----------------------------------------------------------------------------
RETURN
*-----------------------------------------------------------------------------
END
