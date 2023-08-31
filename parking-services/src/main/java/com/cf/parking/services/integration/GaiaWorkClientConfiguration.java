package com.cf.parking.services.integration;

import feign.RequestInterceptor;
import feign.codec.Decoder;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;

import java.util.*;
import java.util.stream.Collectors;

/**
 * @author whx
 * @date 2023/3/27
 */
@Slf4j
public class GaiaWorkClientConfiguration {
	private static final String SECRET_KEY = "cfmotoAppKey";
	@Value("${gateway.cfmotoAppKey.gaiawork}")
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
			headers.put("Accept", "application/json, text/plain, */*");
			//headers.put("Content-Type", "application/x-www-form-urlencoded");
			//headers.put("Content-Type", "application/json");
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
		return new GatewayCustomDecoder();
	}
}
