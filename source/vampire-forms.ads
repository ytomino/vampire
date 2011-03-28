-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Streams;
with Ada.Strings.Unbounded;
with Web;
with Tabula.Casts;
with Tabula.Villages;
package Vampire.Forms is
	
	type Base_Page is (Index_Page, User_Page, User_List_Page, Village_Page);
	
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
		Village_Id : Villages.Village_Id;
		Day : Integer := -1;
		First : Integer := -1;
		Last : Integer := -1;
		Latest : Integer := -1;
		User_Id : String;
		User_Password : String)
		return Web.Query_Strings is abstract;
	
	function Parameters_To_Base_Page (
		Form : Root_Form_Type'Class;
		Base_Page : Forms.Base_Page;
		Village_Id : Villages.Village_Id;
		User_Id : String;
		User_Password : String)
		return Web.Query_Strings;
	
	procedure Write_Attribute_Name (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Name : in String);
	procedure Write_Attribute_Open (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class);
	procedure Write_Attribute_Close (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class);
	
	procedure Write_In_HTML (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Form : in Root_Form_Type;
		Item : in String;
		Pre : in Boolean := False) is abstract;
	
	procedure Write_In_Attribute (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Form : in Root_Form_Type;
		Item : in String) is abstract;
	
	procedure Write_Link (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Form : in Root_Form_Type'Class;
		Current_Directory : in String;
		Resource : in String;
		Parameters : in Web.Query_Strings := Web.String_Maps.Empty_Map);
	
	procedure Write_Link_To_Village_Page (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Form : in Root_Form_Type'Class;
		Current_Directory : in String;
		HTML_Directory : in String;
		Log : in Boolean;
		Village_Id : in Villages.Village_Id;
		Day : Integer := -1;
		First : Integer := -1;
		Last : Integer := -1;
		Latest : Integer := -1;
		User_Id : in String;
		User_Password : in String);
	
	function Paging (Form : Root_Form_Type) return Boolean is abstract;
	function Speeches_Per_Page (Form : Root_Form_Type) return Natural is abstract;
	
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
	
	-- ページ
	
	function Get_Base_Page (
		Form : Root_Form_Type'Class;
		Query_Strings : Web.Query_Strings;
		Cookie : Web.Cookie)
		return Base_Page;
	
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
		return Villages.Village_Id is abstract;
	
	function Get_Day (
		Form : Root_Form_Type;
		Village : Villages.Village_Type'Class;
		Query_Strings : Web.Query_Strings)
		return Natural is abstract;
	
	function Get_Range (
		Form : Root_Form_Type;
		Village : Villages.Village_Type'Class;
		Day : Natural;
		Query_Strings : Web.Query_Strings)
		return Villages.Message_Range_Type is abstract;
	
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
		Work_Index : Casts.Works.Cursor; -- "既定"はNo_Element
		Name_Index : Casts.People.Cursor; -- No_Elementにはならない
		Request : aliased Ada.Strings.Unbounded.Unbounded_String;
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
		return String;
	
	function Get_Action (
		Form : Root_Form_Type'Class;
		Inputs : Web.Query_Strings)
		return String;
	
	function Get_Target (
		Form : Root_Form_Type'Class;
		Inputs : Web.Query_Strings)
		return Villages.Person_Index'Base;
	
	function Get_Special (
		Form : Root_Form_Type'Class;
		Inputs : Web.Query_Strings)
		return Boolean;
	
	procedure Set_Rule (
		Form : in Root_Form_Type'Class;
		Village : in out Villages.Village_Type'Class;
		Inputs : in Web.Query_Strings);
	
end Vampire.Forms;
