package com.cf.parking.services.integration;


import com.hikvision.artemis.sdk.util.SignUtil;
import feign.RequestInterceptor;
import feign.codec.Decoder;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;

import java.util.*;
import java.util.stream.Collectors;

/**
 * @Author caiChengYu
 * @Date 9:16 2022/7/25
 * @Version 1.0
 **/
@Slf4j
public class ClientConfiguration {
    private static final String SECRET_KEY = "cfmotoAppKey";
    @Value("${gateway.cfmotoAppKey.hikvision}")
    private String appKeyValue;

    /**
     * 拦截器，用于添加header
     *
     * @return 拦截器
     */
    @Bean
    public RequestInterceptor headerInterceptor() {

        return template -> {

            Map<String, String> headers = new HashMap<>(8);
            headers.put("Accept", "*/*");
            headers.put("Content-Type", "application/json");
            headers.put("x-ca-timestamp", String.valueOf((new Date()).getTime()));
            headers.put("x-ca-nonce", UUID.randomUUID().toString());
            headers.put("x-ca-key", "23984426");
            headers.put("x-ca-signature", SignUtil.sign("u51bdIluCwbZb9U8CtoF", "POST", template.url(), headers, null, null, null));
            headers.put(SECRET_KEY, appKeyValue);
            Map<String, Collection<String>> convertedMap = headers.entrySet().stream()
                    .collect(Collectors.toMap(
                            Map.Entry::getKey,
                            entry -> Arrays.asList(entry.getValue())
                    ));
            template.headers(convertedMap);
            log.info("Request: {} {}", template.method(), template.url());
            if (template.body() != null) {
                log.info("Body: {}", template.body());
            }
        };


    }

    @Bean
    public Decoder decoder() {
        return new CustomDecoder();
    }






}










