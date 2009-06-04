-- The Village of Vampire by YT, このソースコードはNYSLです
with DYAYaml;
package Tabula.Villages.Village_IO is
	
	Yaml_Type : constant String := "vampire-village";
	
	procedure IO(Serializer : in out DYAYaml.Serializer; Name : String; People : in out Person_Array_Access);
	procedure IO(Serializer: in out DYAYaml.Serializer; Village: in out Village_Type; Info_Only : Boolean := False);
	
	package Sex_Kind_IO is new DYAYaml.IO_Enumeration(Sex_Kind);
	package Person_Sex_IO is new DYAYaml.IO_Enumeration(Person_Sex);
	package Requested_Role_IO is new DYAYaml.IO_Enumeration(Requested_Role);
	package Person_Role_IO is new DYAYaml.IO_Enumeration(Person_Role);
	package Person_State_IO is new DYAYaml.IO_Enumeration(Person_State);
	package Message_Kind_IO is new DYAYaml.IO_Enumeration(Message_Kind);
	package Village_State_IO is new DYAYaml.IO_Enumeration(Village_State);
	package Village_Time_IO is new DYAYaml.IO_Enumeration(Village_Time);
	package Attack_IO is new DYAYaml.IO_Enumeration(Attack_Mode);
	package Servant_Knowing_IO is new DYAYaml.IO_Enumeration(Servant_Knowing_Mode);
	package Monster_Side_IO is new DYAYaml.IO_Enumeration(Monster_Side);
	package Teaming_IO is new DYAYaml.IO_Enumeration(Teaming);
	package Role_Appearance_IO is new DYAYaml.IO_Enumeration(Role_Appearance);
	package Hunter_Silver_Bullet_IO is new DYAYaml.IO_Enumeration(Hunter_Silver_Bullet_Mode);
	package Daytime_Preview_IO is new DYAYaml.IO_Enumeration(Daytime_Preview_Mode);
	package Doctor_Infected_IO is new DYAYaml.IO_Enumeration(Doctor_Infected_Mode);
	package Unfortunate_IO is new DYAYaml.IO_Enumeration(Unfortunate_Mode);
	
end Tabula.Villages.Village_IO;
