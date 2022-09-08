*-----------------------------------------------------------------------------
* <Rating>-7</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE MBL.H.DEBIT.CARD.REQ.FIELDS
*-----------------------------------------------------------------------------
* Subroutine Description:
* THIS TEMPALTE IS USE FOR DEBIT CARD INFORMATION
*-----------------------------------------------------------------------------
* Modification History :
* 15/07/2020 -                            NEW   - Sarowar Mortoza
*                                                     FDS Pvt Ltd
*<doc>
* Template for field definitions routine MBL.H.DEBIT.CARD.REQ.FIELDS
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
* 19/10/07 - EN_10003543
*            New Template changes
*
* 14/11/07 - BG_100015736
*            Exclude routines that are not released
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
