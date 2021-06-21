library(httr)
library(rvest)
library(tidyverse)

categoryDf = 
    httr::GET(
        'https://www.amazon.com/advanced-search/books',
        add_headers(
            'Host' = 'www.amazon.com',
            'User-Agent' = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:89.0) Gecko/20100101 Firefox/89.0',
            'Accept' = 'text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8',
            'Accept-Language' = 'en-US,en;q=0.5',
            'Accept-Encoding' = 'gzip, deflate, br',
            'Referer' = 'https://www.amazon.com/s?i=stripbooks&s=featured-rank&page=75&Adv-Srch-Books-Submit.x=28&Adv-Srch-Books-Submit.y=4&qid=1624215226&unfiltered=1&ref=sr_pg_35',
            'DNT' = '1',
            'Connection' = 'keep-alive',
            'Cookie' = 'session-id=132-0382992-9041163; session-id-time=2082787201l; i18n-prefs=USD; csm-hit=adb:adblk_yes&t:1624216119012&tb:3MYG88W0AWMTQBM0PDP9+s-8CKQVR02J9SDP8GZDY41|1624216119012; ubid-main=132-6908109-5854564; session-token=TuXcWYfNb8GSU81q3R+oqenenhQOfuruv77AQf+E1Ks8JEhdqbI4Es5b8Uoj6hz1j1ATaTGJONkztBTCA4E8Ovg7TfH7FsqQvqua0tz7Yx+JY8Jm3ZJ+FKlwyx/d+EWrnT+nv27NwNQNZgEE0Kh8JwPI++XziXwGxi0wbGx38zrBcPARnZQOn+8OZp45NO4Z; lc-main=en_US; s_fid=04B6FE28559B3130-1D2EBDDA8B14DEE8; regStatus=pre-register; aws-target-visitor-id=1623985856797-420435; aws-target-data=%7B%22support%22%3A%221%22%7D; skin=noskin; s_cc=true; aws-ubid-main=338-1437060-5050188',
            'Upgrade-Insecure-Requests' = '1',
            'Sec-GPC' = '1',
            'Pragma' = 'no-cache',
            'Cache-Control' = 'no-cache',
            'TE' = 'Trailers'
            )
        ) %>%
    httr::content(.) %>%
    html_nodes(., '#asMain select') %>%
    .[1] %>%
    html_nodes(., 'option') %>%
    map_dfr(., function(x) tibble(category = x %>% html_text(.) %>% str_squish(.), id = x %>% html_attr('value'))) %>%
    dplyr::filter(., nchar(id) >= 1) %>%
    dplyr::filter(., category != 'Medicine') # Issue with medicine id not correct -> causing redirect on page load


titleDf =
    categoryDf %>% 
    purrr::transpose(.) %>%
    map_dfr(., function(x) {
        categoryResults =
            map_dfr(1:2, function(page) {
                message('Scraping ',  x$category, ', page ', page, '/', 70)
                httr::GET(
                    paste0('https://www.amazon.com/s?i=stripbooks&rh=n%3A', x$id,'&s=relevanceexprank&fs=true&page=', page,'')
                    ) %>%
                    httr::content(.) %>%
                    html_nodes(., 'div.s-main-slot > div.s-result-item') %>%
                    purrr::map_dfr(., function(y) {
                        tibble(
                            title = y %>% html_node(., 'h2 > a.a-text-normal > span.a-text-normal') %>% html_text(.),
                            version = y %>%  html_node(., 'a.a-size-base.a-link-normal.a-text-bold') %>% html_text(.),
                            url = y %>% html_node(., 'a.a-link-normal.a-text-normal') %>% html_attr('href'),
                            category = x$category,
                            scrapedDirectly = TRUE
                        )
                    }) %>%
                    dplyr::filter(., !is.na(title))
            }) %>%
            dplyr::mutate(., categoryScrapeOrder = 1:nrow(.))
        })


titleVersionDf =
    titleDf %>%
    purrr::transpose(.) %>%
    .[1:100] %>%
    purrr::imap_dfr(., function(x, i) {
        message('Pulling data for book ', i)
        pageData =
            httr::GET(
                paste0('https://www.amazon.com/', x$url),
                add_headers(
                    'Host' = 'www.amazon.com',
                    'User-Agent' = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:89.0) Gecko/20100101 Firefox/89.0',
                    'Accept' = 'text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8',
                    'Accept-Language' = 'en-US,en;q=0.5',
                    'Accept-Encoding' = 'gzip, deflate, br',
                    'Referer' = 'https://www.amazon.com/s?i=stripbooks&s=featured-rank&page=75&Adv-Srch-Books-Submit.x=28&Adv-Srch-Books-Submit.y=4&qid=1624215226&unfiltered=1&ref=sr_pg_35',
                    'DNT' = '1',
                    'Connection' = 'keep-alive',
                    'Cookie' = 'session-id=132-0382992-9041163; session-id-time=2082787201l; i18n-prefs=USD; csm-hit=adb:adblk_yes&t:1624216119012&tb:3MYG88W0AWMTQBM0PDP9+s-8CKQVR02J9SDP8GZDY41|1624216119012; ubid-main=132-6908109-5854564; session-token=TuXcWYfNb8GSU81q3R+oqenenhQOfuruv77AQf+E1Ks8JEhdqbI4Es5b8Uoj6hz1j1ATaTGJONkztBTCA4E8Ovg7TfH7FsqQvqua0tz7Yx+JY8Jm3ZJ+FKlwyx/d+EWrnT+nv27NwNQNZgEE0Kh8JwPI++XziXwGxi0wbGx38zrBcPARnZQOn+8OZp45NO4Z; lc-main=en_US; s_fid=04B6FE28559B3130-1D2EBDDA8B14DEE8; regStatus=pre-register; aws-target-visitor-id=1623985856797-420435; aws-target-data=%7B%22support%22%3A%221%22%7D; skin=noskin; s_cc=true; aws-ubid-main=338-1437060-5050188',
                    'Upgrade-Insecure-Requests' = '1',
                    'Sec-GPC' = '1',
                    'Pragma' = 'no-cache',
                    'Cache-Control' = 'no-cache',
                    'TE' = 'Trailers'
                    )
                ) %>%
            httr::content(., encoding = 'UTF-8')
        
        versionsDf =
            pageData %>%
            html_node('#tmmSwatches') %>%
            html_nodes('a.a-button-text') %>%
            html_text(.) %>%
            str_split(., '\\n') %>%
            imap_dfr(., function(x, i)
                str_squish(x) %>% keep(., ~ nchar(.) >= 1) %>%
                    {tibble(version = .[1], price = str_replace_all(.[2], coll('$'), ''))}
                ) %>%
            dplyr::mutate(
                .,
                url = 
                    pageData %>%
                    html_node('#tmmSwatches') %>%
                    html_nodes('a.a-button-text') %>%
                    html_attr('href') %>%
                    str_replace_all(., coll('javascript:void(0)'), x$url)
                ) %>%
            dplyr::transmute(
                .,
                title = x$title,
                version,
                price,
                url,
                category = x$category,
                scrapedDirectly = (x$version == version),
                categoryScrapeOrder = x$categoryScrapeOrder
                )
        
        })

        

resDf =
    titleVersionDf %>%
    dplyr::filter(., !version %in% c('Audiobook')) %>% # Remove audiobooks since details don't follow usual template
    purrr::transpose(.) %>%
    purrr::imap_dfr(., function(x, i) {
        message('Pulling data for row ', i)
        pageData =
            httr::GET(
                paste0('https://www.amazon.com/', x$url),
                add_headers(
                    'Host' = 'www.amazon.com',
                    'User-Agent' = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:89.0) Gecko/20100101 Firefox/89.0',
                    'Accept' = 'text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8',
                    'Accept-Language' = 'en-US,en;q=0.5',
                    'Accept-Encoding' = 'gzip, deflate, br',
                    'Referer' = 'https://www.amazon.com/s?i=stripbooks&s=featured-rank&page=75&Adv-Srch-Books-Submit.x=28&Adv-Srch-Books-Submit.y=4&qid=1624215226&unfiltered=1&ref=sr_pg_35',
                    'DNT' = '1',
                    'Connection' = 'keep-alive',
                    'Cookie' = 'session-id=132-0382992-9041163; session-id-time=2082787201l; i18n-prefs=USD; csm-hit=adb:adblk_yes&t:1624216119012&tb:3MYG88W0AWMTQBM0PDP9+s-8CKQVR02J9SDP8GZDY41|1624216119012; ubid-main=132-6908109-5854564; session-token=TuXcWYfNb8GSU81q3R+oqenenhQOfuruv77AQf+E1Ks8JEhdqbI4Es5b8Uoj6hz1j1ATaTGJONkztBTCA4E8Ovg7TfH7FsqQvqua0tz7Yx+JY8Jm3ZJ+FKlwyx/d+EWrnT+nv27NwNQNZgEE0Kh8JwPI++XziXwGxi0wbGx38zrBcPARnZQOn+8OZp45NO4Z; lc-main=en_US; s_fid=04B6FE28559B3130-1D2EBDDA8B14DEE8; regStatus=pre-register; aws-target-visitor-id=1623985856797-420435; aws-target-data=%7B%22support%22%3A%221%22%7D; skin=noskin; s_cc=true; aws-ubid-main=338-1437060-5050188',
                    'Upgrade-Insecure-Requests' = '1',
                    'Sec-GPC' = '1',
                    'Pragma' = 'no-cache',
                    'Cache-Control' = 'no-cache',
                    'TE' = 'Trailers'
                )
            ) %>%
            httr::content(., encoding = 'UTF-8')
        
        miscDf =
            {
                tibble(
                    amazonAuthor = pageData %>% html_node(., '#bylineInfo > span.author.notFaded > span.a-declarative > a.a-link-normal') %>% html_text(.),
                    amazonRatings = pageData %>% html_node(., '#acrCustomerReviewText') %>% html_text(.) %>% str_replace_all(., c(',' = '', ' ratings'= '')),
                    amazonDesc = pageData %>% html_node(., '#bookDescription_feature_div div') %>% html_text(.)
                    )
            }
        
        details1Df =
            pageData %>%
            html_nodes(., '#detailBullets_feature_div > ul > li') %>%
            html_text(.) %>%
            iconv(., 'utf8', 'ASCII', sub = '') %>%
            str_replace_all(., '[\r\n]', '' ) %>%
            str_split_fixed(., ':', n = 2) %>%
            as.data.frame(.) %>%
            setNames(., c('name', 'value')) %>%
            tidyr::pivot_wider(.) %>%
            setNames(., paste0('amazon', make.names(colnames(.)) %>% str_replace_all(., coll('.'), ''))) 
        
        salesDf =
            pageData %>%
            html_nodes(., '#detailBulletsWrapper_feature_div > ul') %>%
            .[1] %>%
            html_nodes(., 'span.a-list-item') %>%
            .[1] %>%
            html_text(.) %>%
            str_split(., '\\n') %>%
            .[[1]] %>%
            str_squish(.) %>%
            keep(., str_sub(., 1, 1) == '#') %>%
            str_split_fixed(., ' in ', n = 2) %>%
            as.data.frame(.) %>%
            setNames(., c('Rank', 'Cat')) %>%
            as_tibble(.) %>% {
                if (nrow(.) == 0) tibble()
                else 
                    dplyr::mutate(., category = paste0('', 1:nrow(.))) %>%
                        dplyr::mutate(., Rank = str_replace(Rank, coll('#'), '')) %>%
                        dplyr::mutate(., Rank = str_replace(Rank, coll(','), '')) %>%
                        tidyr::pivot_wider(., names_from = category, values_from = c('Rank', 'Cat'), names_sep = '') %>%
                        setNames(., paste0('amzn', colnames(.)))
            }

        x %>%
            as_tibble(.) %>%
            dplyr::bind_cols(miscDf, details1Df, salesDf) %>%
            return(.)
    }) %>%
    dplyr::mutate(
        .,
        scrapeDate = Sys.Date()
    )