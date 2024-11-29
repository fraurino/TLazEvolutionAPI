{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lazevolution;

{$warn 5023 off : no warning about unused units}
interface

uses
  LazEvolutionAPI, FormQrCode, JsonHelpers, LazEvolutionAPIConsts, 
  LazEvolutionAPIContacts, LazEvolutionAPIGlobals, LazEvolutionAPIInstance, 
  LazEvolutionAPIInstanceList, LazEvolutionAPIMediaMessage, 
  LazEvolutionAPIMessageReturn, LazEvolutionAPIResponse, 
  LazEvolutionAPITextMessage, LazEvolutionAPIWhatsAppNumbers, LazRestClient, 
  superdate, superobject, supertypes, superxmlparser, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('LazEvolutionAPI', @LazEvolutionAPI.Register);
end;

initialization
  RegisterPackage('lazevolution', @Register);
end.
