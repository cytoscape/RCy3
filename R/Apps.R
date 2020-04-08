# ==============================================================================
# Functions for inspecting and managing apps for Cytoscape.
# ------------------------------------------------------------------------------
#' @title Disable App
#'
#' @description Disable an app to effectively remove it from your Cytoscape session
#' without having to uninstall it.
#' @param app Name of app
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' disableApp()
#' }
#' @export
disableApp <- function (app, base.url=.defaultBaseUrl){
    .verifySupportedVersions(1,3.7,base.url)
    msg <- commandsGET(paste0('apps disable app="',app,'"'), base.url=base.url)
    message(msg)
}

# ------------------------------------------------------------------------------
#' @title Enable App
#'
#' @description Enable a previously installed and disabled app in Cytoscape.
#' @param app Name of app
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' enableApp()
#' }
#' @export
enableApp <- function (app, base.url=.defaultBaseUrl){
    .verifySupportedVersions(1,3.7,base.url)
    msg <- commandsGET(paste0('apps enable app="',app,'"'), base.url=base.url)
    message(msg)
}

# ------------------------------------------------------------------------------
#' @title Get App Information
#'
#' @description Retrieve the name, brief description and version of a Cytoscape app.
#' @param app Name of app
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return App name, brief description and version.
#' @examples \donttest{
#' getAppInformation()
#' }
#' @export
getAppInformation <- function (app, base.url=.defaultBaseUrl){
    .verifySupportedVersions(1,3.7,base.url)
    commandsGET(paste0('apps information app="',app,'"'), base.url=base.url)
}

# ------------------------------------------------------------------------------
#' @title Install App
#'
#' @description Installs an app in Cytoscape.
#' @param app Name of app
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' installApp()
#' }
#' @export
installApp <- function (app, base.url=.defaultBaseUrl){
    .verifySupportedVersions(1,3.7,base.url)
    msg <- commandsGET(paste0('apps install app="',app,'"'), base.url=base.url)
    message(msg)
}

# ------------------------------------------------------------------------------
#' @title List Available Apps
#'
#' @description Retrieve a list of apps available for installation in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return A list of app names and latest versions
#' @examples \donttest{
#' getAvailableApps()
#' }
#' @export
getAvailableApps <- function (base.url=.defaultBaseUrl){
    .verifySupportedVersions(1,3.7,base.url)
    commandsGET('apps list available', base.url=base.url)
}

# ------------------------------------------------------------------------------
#' @title List Disabled Apps
#'
#' @description Retrieve list of currently disabled apps in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return A list of disabled app names, versions and statuses
#' @examples \donttest{
#' getDisabledApps()
#' }
#' @export
getDisabledApps <- function (base.url=.defaultBaseUrl){
    .verifySupportedVersions(1,3.7,base.url)
    commandsGET('apps list disabled', base.url=base.url)
}

# ------------------------------------------------------------------------------
#' @title List Installed Apps
#'
#' @description Retrieve list of currently installed apps in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return A list of installed app names, versions and statuses
#' @examples \donttest{
#' getInstalledApps()
#' }
#' @export
getInstalledApps <- function (base.url=.defaultBaseUrl){
    .verifySupportedVersions(1,3.7,base.url)
    commandsGET('apps list installed', base.url=base.url)
}

# ------------------------------------------------------------------------------
#' @title List Uninstalled Apps
#'
#' @description Retrieve list of apps not currently installed in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return A list of uninstalled app names, versions and statuses
#' @examples \donttest{
#' getUninstalledApps()
#' }
#' @export
getUninstalledApps <- function (base.url=.defaultBaseUrl){
    .verifySupportedVersions(1,3.7,base.url)
    commandsGET('apps list uninstalled', base.url=base.url)
}

# ------------------------------------------------------------------------------
#' @title List Apps With Updates
#'
#' @description Retrieve list of currently installed Cytoscape apps with updates available.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return A list of updatable app names, versions and statuses
#' @examples \donttest{
#' getAppUpdates()
#' }
#' @export
getAppUpdates <- function (base.url=.defaultBaseUrl){
    .verifySupportedVersions(1,3.7,base.url)
    commandsGET('apps list updates', base.url=base.url)
}

# ------------------------------------------------------------------------------
#' @title Open App Store Page
#'
#' @description Opens the Cytoscape App Store in a new tab in your default browser.
#' @param app Name of app
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' openAppStore()
#' }
#' @export
openAppStore <- function (app, base.url=.defaultBaseUrl){
    .verifySupportedVersions(1,3.7,base.url)
    msg <- commandsGET(paste0('apps open appstore app="',app,'"'), base.url=base.url)
    message(msg)
}

# ------------------------------------------------------------------------------
#' @title App Status
#'
#' @description Retrieve the current status of a Cytoscape app: Installed, Uninstalled
#' or Disabled.
#' @param app Name of app
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return App name and status
#' @examples \donttest{
#' getAppStatus()
#' }
#' @export
getAppStatus <- function (app, base.url=.defaultBaseUrl){
    .verifySupportedVersions(1,3.7,base.url)
    commandsGET(paste0('apps status app="',app,'"'), base.url=base.url)
}

# ------------------------------------------------------------------------------
#' @title Uninstall App
#'
#' @description Uninstall an app from Cytoscape.
#' @param app Name of app
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' uninstallApp()
#' }
#' @export
uninstallApp <- function (app, base.url=.defaultBaseUrl){
    .verifySupportedVersions(1,3.7,base.url)
    msg <- commandsGET(paste0('apps uninstall app="',app,'"'), base.url=base.url)
    message(msg)
}

# ------------------------------------------------------------------------------
#' @title Update App
#'
#' @description Update a Cytoscape app to the latest available version.
#' @param app Name of app
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' updateApp()
#' }
#' @export
updateApp <- function (app, base.url=.defaultBaseUrl){
    .verifySupportedVersions(1,3.7,base.url)
    msg <- commandsGET(paste0('apps update app="',app,'"'), base.url=base.url)
    message(msg)
}

