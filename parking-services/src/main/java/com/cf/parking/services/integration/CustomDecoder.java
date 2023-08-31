package com.cf.parking.services.integration;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import com.alibaba.fastjson.TypeReference;
import com.cf.parking.facade.dto.HikvisionResult;
import com.cf.support.result.Result;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import feign.FeignException;
import feign.Response;
import feign.Util;
import feign.codec.Decoder;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.NamedThreadLocal;
import org.springframework.http.HttpStatus;

import java.io.IOException;
import java.lang.reflect.Type;
import java.util.List;

@Slf4j
public class CustomDecoder implements Decoder {

    private static final String SUCCESS_CODE = "0";

    @Override
    public Object decode(Response response, Type type) throws IOException, FeignException {
        if (response.status() != 200) {
            return handleErrorResponse(response);
        } else {
            return handleSuccessResponse(response, type);
        }

    }

    private Object handleSuccessResponse(Response response, Type type) throws IOException {
        String body = Util.toString(response.body().asReader());
        log.info("Response: {}", body);
        Result<String> result = JSONObject.parseObject(body, new TypeReference<Result<String>>() {
        });
        HikvisionResult hikvisionResult = JSON.parseObject(result.getData(), type);
        if (!SUCCESS_CODE.equals(hikvisionResult.getCode())) {
            throw new CustomException(HttpStatus.INTERNAL_SERVER_ERROR.value(), hikvisionResult.getMsg());
        }
        return hikvisionResult;
    }

    private Object handleErrorResponse(Response response) throws IOException {
        String body = Util.toString(response.body().asReader());
        log.error("Response: {}", body);
        Result<Result> result = JSONObject.parseObject(body, new TypeReference<Result<Result>>() {
        });
        throw new CustomException(response.status(), result.getMsg());
    }

    public class CustomException extends RuntimeException {

        private final int status;

        public CustomException(int status, String message) {
            super(message);
            this.status = status;
        }

        public int getStatus() {
            return status;
        }

    }
}