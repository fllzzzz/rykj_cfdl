package com.cf.parking.services.integration;

import com.alibaba.fastjson.JSON;
import com.cf.support.utils.DingAlarmUtils;
import com.cf.support.utils.SpringUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.util.EntityUtils;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 * @author whx
 * @date 2023/4/3
 */
@Slf4j
public class DingFacadeClient {

	//生产环境访问钉钉push消息群
	private static final String DING_TALK_RESERVE_URL = "https://oapi.dingtalk.com/robot/send?access_token=585d4a89f30884bd5275a2136b6fd904ce89de935160ea910374447f5c3e2889";

	private static ThreadPoolExecutor executorService = new ThreadPoolExecutor(1, 1,
			1L, TimeUnit.MINUTES,
			new ArrayBlockingQueue<>(10),
			new ThreadPoolExecutor.DiscardPolicy());

	/**
	 * 预约通知
	 *
	 * @param content
	 */
	public static void reservePush(String content) {
		try {
			Thread.sleep(1000);
			executorService.execute(new Runnable() {
				@Override
				public void run() {
					alarm(content);
				}
			});
		} catch (Exception e) {
			e.printStackTrace();
			log.info("ding_test_msg_body_thread_err:{}", e.getMessage());
		}
	}

	private static void alarm(String content) {
		String keyWord = "无正常出入通知";
		String url = DING_TALK_RESERVE_URL;
		String profile = SpringUtils.getActiveProfile();
		if (!"prod".equals(profile)) {
			//非生产环境通知测试报警钉钉群
			url = "https://oapi.dingtalk.com/robot/send?access_token=b667edcd1c1864f63e6a5d44b070ffed462dc6eeb0301808ad5e34f3dbb2c1db";
			keyWord = "无正常出入通知";
		}

		CloseableHttpClient client = HttpClients.createDefault();
		HttpPost httpPost = new HttpPost(url);
		httpPost.setHeader("Content-Type", "application/json");
		Map<String, Object> resultMap = new HashMap<>(2);
		Map<String, String> contentMap = new HashMap<>(1);
		contentMap.put("content", keyWord + "\n" + content);
		resultMap.put("text", contentMap);
		resultMap.put("msgtype", "text");
		String jsonString = JSON.toJSONString(resultMap);
		StringEntity requestEntity = new StringEntity(jsonString, "utf-8");
		httpPost.setEntity(requestEntity);
		HttpResponse httpResponse;
		try {
			httpResponse = client.execute(httpPost);
			EntityUtils.toString(httpResponse.getEntity());
			log.info("ding_test_msg_body:{}", httpResponse.getEntity().toString());
		} catch (IOException e) {
			log.error("调用钉钉警告出错", e);
			DingAlarmUtils.alarmException(keyWord + "\n" + content);
		}
	}
}
