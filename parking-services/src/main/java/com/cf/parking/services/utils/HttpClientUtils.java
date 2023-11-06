package com.cf.parking.services.utils;

import org.apache.http.HttpEntity;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.conn.ssl.NoopHostnameVerifier;
import org.apache.http.conn.ssl.SSLConnectionSocketFactory;
import org.apache.http.conn.ssl.TrustSelfSignedStrategy;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.ssl.SSLContexts;
import org.apache.http.util.EntityUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.CollectionUtils;

import java.nio.charset.Charset;
import java.util.Map;


/**
 * http调用
 *
 * @author think
 */
public class HttpClientUtils {


    private static final Logger logger = LoggerFactory.getLogger(HttpClientUtils.class);

    /**
     * 发送post json请求，忽略https的证书
     *
     * @param url
     * @param data   参数
     * @param header 请求头
     * @return
     */
    public static String postJsonWithoutSSL(String url, String data, Map<String, String> header) throws Exception {
        //创建post请求对象
        HttpPost httpPost = new HttpPost(url);
        String jsonObject = "";
        //创建CloseableHttpClient对象（忽略证书的重点）
        CloseableHttpClient client = null;
        SSLConnectionSocketFactory scsf = new SSLConnectionSocketFactory(
                SSLContexts.custom().loadTrustMaterial(null, new TrustSelfSignedStrategy()).build(),
                NoopHostnameVerifier.INSTANCE);
        client = HttpClients.custom().setSSLSocketFactory(scsf).build();

        if (!CollectionUtils.isEmpty(header)) {
            header.keySet().forEach(key -> {
                //设置请求头
                httpPost.addHeader(key, header.get(key));
            });
        }
        httpPost.addHeader("Content-Type", "application/json");
        httpPost.setEntity(new StringEntity(data, Charset.forName("utf-8")));
        //使用CloseableHttpClient发送请求
        CloseableHttpResponse response = client.execute(httpPost);
        HttpEntity entity = response.getEntity();
        jsonObject = EntityUtils.toString(entity, "UTF-8");
        return jsonObject;
    }


}
