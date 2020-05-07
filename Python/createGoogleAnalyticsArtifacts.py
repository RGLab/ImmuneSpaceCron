"""Hello Analytics Reporting API V3."""
"""Does not appear to work with V4 for authentication"""
import pandas as pd

from apiclient.discovery import build
from oauth2client.service_account import ServiceAccountCredentials


def get_service(api_name, api_version, scopes, key_file_location):
    """Get a service that communicates to a Google API.

    Args:
        api_name: The name of the api to connect to.
        api_version: The api version to connect to.
        scopes: A list auth scopes to authorize for the application.
        key_file_location: The path to a valid service account JSON key file.

    Returns:
        A service that is connected to the specified API.
    """

    credentials = ServiceAccountCredentials.from_json_keyfile_name(
            key_file_location, scopes=scopes)

    # Build the service object.
    service = build(api_name, api_version, credentials=credentials)

    return service


def get_production_profile_id(service):
    # Use the Analytics service object to get the first profile id.

    # Get a list of all Google Analytics accounts for this user
    accounts = service.management().accounts().list().execute()

    if accounts.get('items'):
        # Get the first Google Analytics account.
        account = accounts.get('items')[0].get('id')

        # Get a list of all the properties for the first account.
        properties = service.management().webproperties().list(
                accountId=account).execute()

        if properties.get('items'):
            # Get the first property id.
            property = properties.get('items')[0].get('id')

            # Get a list of all views (profiles) for the first property.
            profiles = service.management().profiles().list(
                    accountId=account,
                    webPropertyId=property).execute()

            if profiles.get('items'):
                # return the production profile
                names = []
                items = profiles.get('items')
                for item in items: 
                  names.append(item.get('name'))
                prodIndex = names.index('PROD')
                return profiles.get('items')[prodIndex].get('id')

    return None


def get_results(service, profile_id):
    # Use the Analytics Service Object to query the Core Reporting API
    # for the number of sessions within the past seven days.

    # Dimensions - Sources - ga:source, ga:fullReferrer, ga:country, ga:pagePath
    # Metrics (summarized by dimensions) - ga:users, ga:sessions, ga:bounces
    

    return service.data().ga().get(
            ids='ga:' + profile_id,
            start_date='30daysAgo',
            end_date='today',
            dimensions='ga:source, ga:fullReferrer, ga:country, ga:landingPagePath, ga:secondPagePath',
            metrics='ga:sessions, ga:bounces, ga:users').execute()


def munge_results(results):
    # # Print data nicely for the user.
    # if results:
    #     # print 'View (Profile):', results.get('profileInfo').get('profileName')
    #     # print 'Total Sessions:', results.get('rows')[0][0]
    #     print(results.get('rows'))

    # else:
    #     print 'No results found'

    # turn results.get('rows') into a data.frame with pandas to print out as a matrix
    # strip the leading "u" from each element
    df = pd.DataFrame(results.get('rows'), columns = ['source', 'fullReferrer', 'country', 'landingPage', 'secondPage', 'sessions', 'bounces', 'users'])
    return(df)

def save_df(df):
  df.to_csv('~/Documents/FHCRC/ImmuneSpaceCronjobs/Python/test.csv')

def main():
    # Define the auth scopes to request.
    scope = ['https://www.googleapis.com/auth/analytics.readonly']
    key_file_location = '/home/evanhenrich/Documents/FHCRC/tmp_work/ISmonitor-5c21f4187a27.json'

    # Authenticate and construct service.
    service = get_service(
            api_name='analytics',
            api_version='v3',
            scopes=scope,
            key_file_location=key_file_location)

    profile_id = get_production_profile_id(service)
    df = munge_results(get_results(service, profile_id))
    save_df(df)


if __name__ == '__main__':
  main()
