-- The Village of Vampire by YT, このソースコードはNYSLです
with Web;
with Tabula.Villages;
with Vampire.Villages;
package Vampire.Forms is
	
	type Root_Form_Type is abstract tagged limited null record;
	
	-- ユーザー情報
	
	function Get_User_Id (
		Form : Root_Form_Type;
		Query_Strings : Web.Query_Strings;
		Cookie : Web.Cookie)
		return String is
		abstract;
	
	function Get_User_Password (
		Form : Root_Form_Type;
		Query_Strings : Web.Query_Strings;
		Cookie : Web.Cookie)
		return String is
		abstract;
	
	procedure Set_User (
		Form : in out Root_Form_Type;
		Cookie : in out Web.Cookie;
		New_User_Id: in String;
		New_User_Password : in String) is
		abstract;
	
	-- 個人ページ
	
	function Is_User_Page (
		Form : Root_Form_Type;
		Query_Strings : Web.Query_Strings;
		Cookie : Web.Cookie)
		return Boolean is
		abstract;
	
	-- 村
	
	function Get_Village_Id (
		Form : Root_Form_Type;
		Query_Strings : Web.Query_Strings)
		return Tabula.Villages.Village_Id is
		abstract;
	
	function Get_Day (
		Form : Root_Form_Type;
		Village : Villages.Village_Type; 
		Query_Strings : Web.Query_Strings)
		return Natural is
		abstract;
	
	type Message_Range is record
	   First, Last : Integer;
	end record;
	
	function Get_Range (
		Form : Root_Form_Type;
		Village : Villages.Village_Type; 
		Day : Natural;
		Query_Strings : Web.Query_Strings)
		return Message_Range is
		abstract;
	
	function Get_Text (
		Form : Root_Form_Type;
		Inputs : Web.Query_Strings)
		return String is
		abstract;
	
end Vampire.Forms;
