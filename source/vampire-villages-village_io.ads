-- The Village of Vampire by YT, このソースコードはNYSLです
with Serialization;
package Vampire.Villages.Village_IO is
	
	Yaml_Type : constant String := "vampire-village";
	
	procedure IO (
		Serializer : not null access Serialization.Serializer;
		Name : in String; People : in out Villages.People.Vector);
	procedure IO (
		Serializer : not null access Serialization.Serializer;
		Village : in out Village_Type;
		Info_Only : in Boolean := False);
	
	package Requested_Role_IO is new Serialization.IO_Enumeration (Requested_Role);
	package Person_Role_IO is new Serialization.IO_Enumeration (Person_Role);
	package Person_State_IO is new Serialization.IO_Enumeration (Person_State);
	package Message_Kind_IO is new Serialization.IO_Enumeration (Message_Kind);
	package Village_Time_IO is new Serialization.IO_Enumeration (Village_Time);
	package Role_Appearance_IO is
		new Serialization.IO_Enumeration (Role_Appearance);
	
	package Vote_IO is new Serialization.IO_Enumeration (Vote_Mode);
	package Execution_IO is new Serialization.IO_Enumeration (Execution_Mode);
	package Formation_IO is new Serialization.IO_Enumeration (Formation_Mode);
	package Monster_Side_IO is
		new Serialization.IO_Enumeration (Monster_Side_Mode);
	package Attack_IO is new Serialization.IO_Enumeration (Attack_Mode);
	package Vampire_Action_Set_IO is
		new Serialization.IO_Enumeration (Vampire_Action_Set_Mode);
	package Servant_Knowing_IO is
		new Serialization.IO_Enumeration (Servant_Knowing_Mode);
	package Daytime_Preview_IO is
		new Serialization.IO_Enumeration (Daytime_Preview_Mode);
	package Doctor_Infected_IO is
		new Serialization.IO_Enumeration (Doctor_Infected_Mode);
	package Hunter_Silver_Bullet_IO is
		new Serialization.IO_Enumeration (Hunter_Silver_Bullet_Mode);
	package Unfortunate_IO is new Serialization.IO_Enumeration (Unfortunate_Mode);
	package Obsolete_Teaming_IO is
		new Serialization.IO_Enumeration (Obsolete_Teaming_Mode);
	
end Vampire.Villages.Village_IO;
