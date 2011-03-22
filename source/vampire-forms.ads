-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Streams;
with Web;
with Tabula.Casts;
with Tabula.Villages;
with Vampire.Villages;
package Vampire.Forms is
	
	type Role_Images is array (Villages.Person_Role) of not null access constant String;
	
	type Template_Set_Type is (For_Full, For_Mobile);
	
	type Root_Form_Type is abstract tagged limited null record;
	
	-- HTML / template set
	
	function HTML_Version (Form : Root_Form_Type) return Web.HTML_Version is abstract;
	function Template_Set (Form : Root_Form_Type) return Template_Set_Type is abstract;
	
	-- 出力用
	
	function Self return String;
	
	function Parameters_To_Index_Page (
		Form : Root_Form_Type;
		User_Id : String;
		User_Password : String)
		return Web.Query_Strings is abstract;
	
	function Parameters_To_User_Page (
		Form : Root_Form_Type;
		User_Id : String;
		User_Password : String)
		return Web.Query_Strings is abstract;
	
	function Parameters_To_Village_Page (
		Form : Root_Form_Type;
		Village_Id : Tabula.Villages.Village_Id;
		Day : Integer := -1;
		First : Integer := -1;
		Last : Integer := -1;
		Latest : Integer := -1;
		User_Id : String;
		User_Password : String)
		return Web.Query_Strings is abstract;
	
	procedure Write_In_HTML (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Form : in Root_Form_Type;
		Item : in String;
		Pre : in Boolean := False) is abstract;
	
	procedure Write_In_Attribute (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Form : in Root_Form_Type;
		Item : in String) is abstract;
	
	procedure Write_Link_To_Resource (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Form : in Root_Form_Type'Class;
		Current_Directory : in String;
		Resource : in String;
		Parameters : in Web.Query_Strings := Web.String_Maps.Empty_Map);
	
	procedure Write_Link_To_Index_Page (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Form : in Root_Form_Type'Class;
		Current_Directory : in String;
		User_Id : in String;
		User_Password : in String);
	
	procedure Write_Link_To_User_Page (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Form : in Root_Form_Type'Class;
		Current_Directory : in String;
		User_Id : in String;
		User_Password : in String);
	
	procedure Write_Link_To_Village_Page (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Form : in Root_Form_Type'Class;
		Current_Directory : in String;
		HTML_Directory : in String;
		Log : in Boolean;
		Village_Id : Tabula.Villages.Village_Id;
		Day : Integer := -1;
		First : Integer := -1;
		Last : Integer := -1;
		Latest : Integer := -1;
		User_Id : in String;
		User_Password : in String);
	
	-- ユーザー情報
	
	function Get_User_Id (
		Form : Root_Form_Type;
		Query_Strings : Web.Query_Strings;
		Cookie : Web.Cookie)
		return String is abstract;
	
	function Get_User_Password (
		Form : Root_Form_Type;
		Query_Strings : Web.Query_Strings;
		Cookie : Web.Cookie)
		return String is abstract;
	
	function Get_New_User_Id (
		Form : Root_Form_Type'Class;
		Inputs : Web.Query_Strings)
		return String;
	
	function Get_New_User_Password (
		Form : Root_Form_Type'Class;
		Inputs : Web.Query_Strings)
		return String;
	
	function Get_New_User_Confirmation_Password (
		Form : Root_Form_Type'Class;
		Inputs : Web.Query_Strings)
		return String;
	
	procedure Set_User (
		Form : in out Root_Form_Type;
		Cookie : in out Web.Cookie;
		New_User_Id: in String;
		New_User_Password : in String) is abstract;
	
	-- 個人ページ
	
	function Is_User_Page (
		Form : Root_Form_Type;
		Query_Strings : Web.Query_Strings;
		Cookie : Web.Cookie)
		return Boolean is abstract;
	
	function Is_User_List_Page (
		Form : Root_Form_Type'Class;
		Query_Strings : Web.Query_Strings)
		return Boolean;
	
	-- 村
	
	function Get_Village_Id (
		Form : Root_Form_Type;
		Query_Strings : Web.Query_Strings)
		return Tabula.Villages.Village_Id is abstract;
	
	function Get_Day (
		Form : Root_Form_Type;
		Village : Villages.Village_Type; 
		Query_Strings : Web.Query_Strings)
		return Natural is abstract;
	
	type Message_Range is record
	   First, Last : Integer;
	end record;
	
	function Get_Range (
		Form : Root_Form_Type;
		Village : Villages.Village_Type; 
		Day : Natural;
		Query_Strings : Web.Query_Strings)
		return Message_Range is abstract;
	
	-- コマンド
	
	function Get_Command (
		Form : Root_Form_Type'Class;
		Inputs : Web.Query_Strings)
		return String;
	
	function Get_New_Village_Name (
		Form : Root_Form_Type;
		Inputs : Web.Query_Strings)
		return String is abstract;
	
	type Joining is record
		Work_Index : Tabula.Casts.Works.Cursor; -- "既定"はNo_Element
		Name_Index : Tabula.Casts.People.Cursor; -- No_Elementにはならない
		Request : Villages.Requested_Role;
	end record;
	
	function Get_Joining (
		Form : Root_Form_Type;
		Inputs : Web.Query_Strings)
		return Joining;
	
	type Mark is (Missing, NG, OK);
	
	function Get_Answered (
		Form : Root_Form_Type;
		Inputs : Web.Query_Strings)
		return Mark;
	
	function Get_Text (
		Form : Root_Form_Type;
		Inputs : Web.Query_Strings)
		return String is abstract;
	
	function Get_Reedit_Kind (
		Form : Root_Form_Type'Class;
		Inputs : Web.Query_Strings)
		return Villages.Message_Kind;
	
	function Get_Action (
		Form : Root_Form_Type'Class;
		Inputs : Web.Query_Strings)
		return String;
	
	function Get_Target (
		Form : Root_Form_Type'Class;
		Inputs : Web.Query_Strings)
		return Villages.People.Cursor;
	
	function Get_Special (
		Form : Root_Form_Type'Class;
		Inputs : Web.Query_Strings)
		return Boolean;
	
	procedure Set_Rule (
		Form : in Root_Form_Type'Class;
		Village : in out Villages.Village_Type;
		Inputs : in Web.Query_Strings);
	
end Vampire.Forms;
