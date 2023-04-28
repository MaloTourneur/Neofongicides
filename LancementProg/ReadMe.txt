# Lancement de l'interface graphique sans Rstudio

Pour pouvoir lancer l'interface graphique sans passer par Rstudio, vous devez suivre les instructions suivantes :

## 1. Ouvrir le fichier RunApp.R

Changer le contenu de la variable `folder_address` par le chemin vers le fichier `ApplicationPrincipale.R`. Par exemple : `C:/ProjetM1/ProgrammePrincipal/ApplicationPrincipale.R`

```R
require(shiny)
folder_address = '******'
runApp(folder_address, launch.browser=TRUE)

## 2. Renommer le fichier RunApp.bat en un fichier RunApp.txt

Faites un clic droit sur le fichier RunApp.bat.
Appuyez sur **Renommer**.
Changez `.bat` en `.txt`.

## 3. Ouvrir RunApp.txt

Changez le chemin vers le fichier `R.exe`. Par exemple : `"C:\R-4.2.3\bin\R.exe" CMD BATCH "..\R\RunApp.R"`

## 4. Renommez le fichier RunApp.txt en un fichier RunApp.bat.

## 5. Vous pouvez maintenant lancer l'interface graphique sans passer par Rstudio :

Ouvrez le fichier RunApp.bat.